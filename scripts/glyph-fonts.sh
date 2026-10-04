#!/usr/bin/env bash
# Which font renders each glyph the game can draw?
#
# Enumerates the game's full glyph vocabulary (via the `glyph_vocabulary`
# debug binary, which gathers it from the real render sources) and asks the
# local font stack which font it would use for each codepoint.
#
# On Linux it emulates the terminal by querying fontconfig *with the configured
# family*:
#   fc-match -f '%{family}\t%{file}' '<family>:charset=<U+XXXX>'
# fontconfig returns the named family when it has the glyph, and a fallback
# otherwise; a result whose family differs from the configured one is marked
# `fallback`. Without fc-match it falls back to `floating_square_debug
# which-font`, which lists a font containing the glyph (no OS first pick).
#
# Usage:
#   scripts/glyph-fonts.sh                        # every glyph in the game
#   scripts/glyph-fonts.sh ❶ ◾                   # just these characters
#   scripts/glyph-fonts.sh U+2776 U+24EB          # or code points
#   scripts/glyph-fonts.sh --files                # add the font file path
#   scripts/glyph-fonts.sh --all U+2776           # ranked candidates per glyph
#   scripts/glyph-fonts.sh --family 'DejaVu Sans' # a different configured font
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."

DEFAULT_FAMILY='CaskaydiaMono Nerd Font'

family="$DEFAULT_FAMILY"
show_files=false
show_all=false
glyph_args=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        --family)
            [[ $# -ge 2 ]] || { echo "--family needs a value" >&2; exit 2; }
            family="$2"; shift 2 ;;
        --family=*) family="${1#*=}"; shift ;;
        --files) show_files=true; shift ;;
        --all) show_all=true; shift ;;
        --) shift; glyph_args+=("$@"); break ;;
        -h|--help)
            cat <<'EOF'
usage: scripts/glyph-fonts.sh [--family NAME] [--files] [--all] [chars...] [U+XXXX...]
  (no args)      every glyph the game can draw
  chars/U+XXXX   only the given characters / code points
  --family NAME  configured font family for the fontconfig query
                 (default: CaskaydiaMono Nerd Font)
  --files        also show the resolved font file path
  --all          also show the ranked candidates for each glyph
EOF
            exit 0 ;;
        *) glyph_args+=("$1"); shift ;;
    esac
done

vocabulary() {
    cargo run -q -p game --bin glyph_vocabulary --features debug-tools -- "$@"
}

if (( ${#glyph_args[@]} > 0 )); then
    lines=$(vocabulary "${glyph_args[@]}")
else
    lines=$(vocabulary)
fi

have_fc_match=false
command -v fc-match >/dev/null 2>&1 && have_fc_match=true

primary_family=""
if $have_fc_match; then
    primary_family=$(fc-match -f '%{family}' "$family" | cut -d, -f1)
fi

declare -A count_by_font=()
declare -A sample_by_font=()
declare -A font_status=()
n=0
n_fallback=0

if $show_files; then
    printf '%-10s %-3s %-8s %-30s %s\n' CODEPOINT GLYPH STATUS FONT FILE
else
    printf '%-10s %-3s %-8s %s\n' CODEPOINT GLYPH STATUS FONT
fi
while IFS= read -r line; do
    [[ -z "$line" ]] && continue
    cp="${line%% *}"
    ch="${line#* }"
    [[ "$ch" == "$cp" ]] && ch="?"
    hex="${cp#U+}"

    fam=""; file=""
    if $have_fc_match; then
        out=$(fc-match -f '%{family}\t%{file}' "$family:charset=${hex}" 2>/dev/null || true)
        fam="${out%%$'\t'*}"
        file="${out#*$'\t'}"
        [[ "$file" == "$out" ]] && file=""
        fam="$(printf '%s' "$fam" | cut -d, -f1)"
    else
        # which-font lists a containing font; no OS first-pick ordering.
        fam=$(cargo run -q -p floating_square_debug -- which-font "$cp" 2>/dev/null \
            | awk '/^scanned|^looking|^no installed/ { next } /^[^ ]/ { sub(/  \[.*/, ""); print; exit }')
    fi
    fam="${fam:-<none>}"

    status="unknown"
    if $have_fc_match; then
        status="primary"
        if [[ -n "$primary_family" && "$fam" != "$primary_family" ]]; then
            status="fallback"
            n_fallback=$((n_fallback + 1))
        fi
    fi
    font_status["$fam"]="$status"

    if $show_files; then
        printf '%-10s %-3s %-8s %-30s %s\n' "$cp" "$ch" "$status" "$fam" "$file"
    else
        printf '%-10s %-3s %-8s %s\n' "$cp" "$ch" "$status" "$fam"
    fi

    if $show_all && $have_fc_match; then
        while IFS=$'\t' read -r cand_fam cand_file; do
            [[ -z "$cand_fam" ]] && continue
            printf '    - %s  %s\n' "${cand_fam%%,*}" "$cand_file"
        done < <(fc-match -s -f '%{family}\t%{file}\n' "$family:charset=${hex}" 2>/dev/null || true)
    fi

    count_by_font["$fam"]=$(( ${count_by_font["$fam"]:-0} + 1 ))
    if (( ${count_by_font["$fam"]} <= 16 )); then
        sample_by_font["$fam"]+="$ch"
    fi
    n=$((n + 1))
done <<< "$lines"

echo
if $have_fc_match; then
    echo "configured family: $family -> $primary_family"
    echo "$n glyphs: $((n - n_fallback)) in the configured font, $n_fallback from fallbacks"
else
    echo "note: fc-match not found; listed a containing font per glyph, not the OS first pick"
    echo "$n glyphs (status unknown without fc-match)"
fi
echo
while IFS= read -r fam; do
    count="${count_by_font[$fam]}"
    sample="${sample_by_font[$fam]:-}"
    suffix=""
    (( count > 16 )) && suffix="…"
    printf '  %-30s %4d %-12s %s%s\n' "$fam" "$count" "(${font_status[$fam]})" "$sample" "$suffix"
done < <(printf '%s\n' "${!count_by_font[@]}" | sort)
