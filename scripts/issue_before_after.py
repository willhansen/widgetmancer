#!/usr/bin/env python3
"""Render before/after screen captures to PNGs for issue review.

OpenCode's `read` tool displays PNGs inline, so this turns a fix into a
before/after/marked image triple the reporter can approve or reject:

    scripts/issue_before_after.py \
        --before issues/0026/snapshot/screen.txt \
        --after  /tmp/after.ansi \
        --flags  issues/solved/0026/flags.json \
        --out    /tmp/0026

Writes <out>.before.png, <out>.before-marked.png (flagged squares outlined) and
<out>.after.png.

A screen capture is ANSI: one cell is
``ESC[48;2;R;G;Bm ESC[38;2;R;G;Bm <glyph> ESC[49m ESC[39m``. Each screen square
is two such character columns.
"""

import argparse
import json
import re
import struct
import sys
import zlib

CELL = re.compile(
    r"\x1b\[48;2;(\d+);(\d+);(\d+)m"
    r"\x1b\[38;2;(\d+);(\d+);(\d+)m"
    r"(.*?)\x1b\[49m\x1b\[39m",
    re.S,
)


def parse_screen(text):
    """Return rows of (char, fg, bg) character-column cells."""
    rows = []
    for line in text.split("\n"):
        cells = []
        for m in CELL.finditer(line):
            bg = tuple(int(m.group(i)) for i in (1, 2, 3))
            fg = tuple(int(m.group(i)) for i in (4, 5, 6))
            cells.append((m.group(7), fg, bg))
        if cells:
            rows.append(cells)
    return rows


def png_bytes(width, height, pixels):
    def chunk(tag, data):
        body = struct.pack(">I", len(data)) + tag + data
        return body + struct.pack(">I", zlib.crc32(tag + data) & 0xFFFFFFFF)

    raw = b"".join(
        b"\x00" + bytes(pixels[y * width * 3 : (y + 1) * width * 3])
        for y in range(height)
    )
    png = b"\x89PNG\r\n\x1a\n"
    png += chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0))
    png += chunk(b"IDAT", zlib.compress(raw, 9))
    png += chunk(b"IEND", b"")
    return png


def write_png(path, width, height, pixels):
    with open(path, "wb") as handle:
        handle.write(png_bytes(width, height, pixels))


def visible_color(char, fg, bg):
    """Pick the color that best shows the glyph."""
    if char.strip():
        return fg
    return bg


def render(rows, out, scale, marks):
    data = png_bytes_for(rows, scale, marks)
    with open(out, "wb") as handle:
        handle.write(data)
    return data


def png_bytes_for(rows, scale, marks):
    height = len(rows)
    width = max((len(row) for row in rows), default=0)
    if width == 0:
        raise SystemExit("no cells parsed (not a screen capture?)")

    pixels = bytearray(width * scale * height * scale * 3)

    def put(px, py, color):
        for y in range(py * scale, (py + 1) * scale):
            for x in range(px * scale, (px + 1) * scale):
                i = (y * width * scale + x) * 3
                pixels[i : i + 3] = bytes(color)

    for py, row in enumerate(rows):
        for px, (char, fg, bg) in enumerate(row):
            put(px, py, visible_color(char, fg, bg))

    # Outline the flagged squares (screen squares -> character columns).
    for x0, y0, x1, y1 in marks:
        for cy in range(y0, y1 + 1):
            for cx in (2 * x0, 2 * (x1 + 1) - 1):
                put(cx, cy, MARK)
        for cx in range(2 * x0, 2 * x1 + 2):
            for cy in (y0, y1):
                put(cx, cy, MARK)

    return png_bytes(width * scale, height * scale, pixels)


MARK = (255, 0, 255)

# Luminance ramp for the terminal fallback (one character per screen square).
RAMP = " .:-=+*#%@"


def _cell(square):
    """Representative (char, fg, bg) for a screen square: its left cell."""
    return square


def render_text(rows, marks, label):
    """Plain text art of the screen (works in any terminal): one character per
    screen square by luminance, with the flagged rectangles drawn as a box."""
    grid = []
    for row in rows:
        line = []
        for x in range(len(row) // 2):
            char, fg, bg = row[2 * x]
            color = fg if char.strip() else bg
            level = (color[0] + color[1] + color[2]) / 3.0
            line.append(RAMP[min(len(RAMP) - 1, int(level / 256.0 * len(RAMP)))])
        grid.append(line)
    for (x0, y0, x1, y1) in marks:
        x1 = min(x1, len(grid[0]) - 1)
        y1 = min(y1, len(grid) - 1)
        for x in range(x0, x1 + 1):
            grid[y0][x] = "-"
            grid[y1][x] = "-"
        for y in range(y0, y1 + 1):
            grid[y][x0] = "|"
            grid[y][x1] = "|"
        grid[y0][x0] = grid[y0][x1] = grid[y1][x0] = grid[y1][x1] = "+"
    return f"== {label} ==\n" + "\n".join("".join(row) for row in grid)



def load_flags(path):
    """Screen-square bounds (x0, y0, x1, y1) from a flags.json or issue dir."""
    if path is None:
        return []
    data = json.load(open(path))
    rects = []
    for entry in data:
        screen = entry.get("screen") or entry.get("screen_region")
        if not screen:
            continue
        (x0, y0), (x1, y1) = screen
        rects.append((x0, y0, x1, y1))
    return rects


def crop_to(rows, marks, margin):
    """Slice rows/marks to the flagged bounding box (in screen squares)."""
    if not marks:
        return rows, marks
    x0 = min(m[0] for m in marks) - margin
    y0 = min(m[1] for m in marks) - margin
    x1 = max(m[2] for m in marks) + margin
    y1 = max(m[3] for m in marks) + margin
    x0 = max(x0, 0)
    y0 = max(y0, 0)
    rows = rows[y0 : y1 + 1]
    rows = [row[2 * x0 : 2 * (x1 + 1)] for row in rows]
    shifted = [(m[0] - x0, m[1] - y0, m[2] - x0, m[3] - y0) for m in marks]
    return rows, shifted


def render_ansi(rows, marks, label):
    """Truecolor ANSI half-block art: one character per cell column and two
    rows per character, each pixel a real 24-bit colour. No image protocol, so
    it works through tmux/SSH in any truecolor terminal."""
    def cell_color(row, x):
        char, fg, bg = row[x]
        return fg if char.strip() else bg

    border = set()
    for (x0, y0, x1, y1) in marks:
        for y in range(y0, y1 + 1):
            for x in (2 * x0, 2 * (x1 + 1) - 1):
                border.add((x, y))
        for x in range(2 * x0, 2 * x1 + 2):
            for y in (y0, y1):
                border.add((x, y))

    out = [f"== {label} =="]
    for y in range(0, len(rows) - 1, 2):
        top, bot = rows[y], rows[y + 1]
        n = min(len(top), len(bot))
        line = []
        for x in range(n):
            tc = MARK if (x, y) in border else cell_color(top, x)
            bc = MARK if (x, y + 1) in border else cell_color(bot, x)
            line.append(
                f"\x1b[38;2;{tc[0]};{tc[1]};{tc[2]}m"
                f"\x1b[48;2;{bc[0]};{bc[1]};{bc[2]}m\u2580"
            )
        line.append("\x1b[0m")
        out.append("".join(line))
    return "\n".join(out)


def changed_cells(before, after, marks):
    """Summary of flagged squares that differ, grouped per flag rectangle.

    A no-image-terminal / colour-blind view of exactly what the fix changed:
    how many cells moved between which backgrounds, plus any glyph changes.
    """
    lines = ["== changed cells (grouped per flagged square) =="]
    rows = min(len(before), len(after))
    for (x0, y0, x1, y1) in marks:
        transitions = {}
        glyph_changes = []
        for y in range(y0, min(y1, rows - 1) + 1):
            for x in range(x0, min(x1, len(after[y]) // 2 - 1) + 1):
                for i in range(2):
                    b = before[y][2 * x + i]
                    a = after[y][2 * x + i]
                    if b == a:
                        continue
                    key = (b[2], a[2])
                    transitions[key] = transitions.get(key, 0) + 1
                    if b[0] != a[0]:
                        glyph_changes.append((x, y, i, b[0], a[0]))
        lines.append(f"  flag screen({x0},{y0})..({x1},{y1}):")
        if not transitions:
            lines.append("    (unchanged)")
        for (bg_before, bg_after), count in sorted(
            transitions.items(), key=lambda kv: -kv[1]
        ):
            were = "starfield" if bg_before == (0, 0, 0) else "terrain"
            now = "untinted" if bg_after == (0, 0, 0) else "portal-tinted"
            lines.append(f"    {count:3d} cells  bg{bg_before} -> bg{bg_after}"
                         f"   ({were} -> {now})")
        for x, y, i, g_before, g_after in glyph_changes[:10]:
            lines.append(f"    glyph sq({x},{y})[{i}]: {g_before!r} -> {g_after!r}")
    return "\n".join(lines)


def write_html(path, title, images):
    """Self-contained HTML report: the review images base64-embedded, so it can
    be opened in any browser with no server and no terminal image support."""
    import base64
    import html

    cards = "\n".join(
        f'<figure><figcaption>{html.escape(label)}</figcaption>'
        f'<img alt="{html.escape(label)}" '
        f'src="data:image/png;base64,{base64.b64encode(data).decode()}"></figure>'
        for label, data in images
    )
    doc = f"""<!doctype html>
<html lang="en">
<meta charset="utf-8">
<title>{html.escape(title)}</title>
<style>
 body {{ background:#111; color:#e6e6e6; font:14px/1.4 system-ui,sans-serif; margin:16px; }}
 h1 {{ font-size:16px; }}
 .row {{ display:flex; gap:16px; flex-wrap:wrap; align-items:flex-start; }}
 figure {{ margin:0; }}
 figcaption {{ font-weight:600; margin-bottom:6px; }}
 img {{ image-rendering: pixelated; max-width:100%; border:1px solid #444; }}
</style>
<h1>{html.escape(title)}</h1>
<p>Left to right / top to bottom: before, before with the flagged squares
outlined in magenta, after.</p>
<div class="row">
{cards}
</div>
</html>
"""
    with open(path, "w", encoding="utf-8") as handle:
        handle.write(doc)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--before", required=True)
    parser.add_argument("--after", required=True)
    parser.add_argument("--flags")
    parser.add_argument("--out", default="/tmp/review")
    parser.add_argument("--scale", type=int, default=6)
    parser.add_argument(
        "--crop",
        action="store_true",
        help="crop to the flagged squares plus --margin",
    )
    parser.add_argument("--margin", type=int, default=5)
    parser.add_argument(
        "--text",
        action="store_true",
        help="print plain text art to stdout instead of writing PNGs",
    )
    parser.add_argument(
        "--ansi",
        action="store_true",
        help="print truecolor ANSI half-block art to stdout (no image protocol)",
    )
    parser.add_argument(
        "--html",
        action="store_true",
        help="write a self-contained HTML report (<out>.html) with the images "
        "embedded, to open in a browser",
    )
    parser.add_argument("--title", default="issue review")
    args = parser.parse_args()

    marks = load_flags(args.flags)
    before = parse_screen(open(args.before, encoding="utf-8", errors="replace").read())
    after = parse_screen(open(args.after, encoding="utf-8", errors="replace").read())
    before_full, after_full, marks_full = before, after, marks

    if args.crop:
        before, before_marks = crop_to(before, marks, args.margin)
        after, after_marks = crop_to(after, marks, args.margin)
    else:
        before_marks = after_marks = marks

    if args.text:
        print(render_text(before, before_marks, "BEFORE (flagged squares boxed)"))
        print()
        print(render_text(after, after_marks, "AFTER"))
        print()
        print(changed_cells(before_full, after_full, marks_full))
        return

    if args.ansi:
        print(render_ansi(before, before_marks, "BEFORE (flagged squares boxed)"))
        print()
        print(render_ansi(after, after_marks, "AFTER"))
        print()
        print(changed_cells(before_full, after_full, marks_full))
        return

    # Build the three images. PNGs are always written; --html also embeds them.
    images = [
        ("before", png_bytes_for(before, args.scale, [])),
        ("before-marked", png_bytes_for(before, args.scale, before_marks)),
        ("after", png_bytes_for(after, args.scale, after_marks)),
    ]
    for (label, data) in images:
        with open(f"{args.out}.{label}.png", "wb") as handle:
            handle.write(data)

    if args.html:
        write_html(f"{args.out}.html", args.title, images)
        print(f"wrote {args.out}.html (open it in a browser)")
        print(changed_cells(before_full, after_full, marks_full))
    else:
        print(
            "wrote "
            + "  ".join(f"{args.out}.{label}.png" for label, _ in images)
        )


if __name__ == "__main__":
    main()
