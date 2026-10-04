#!/usr/bin/env bash
# Collect every font the game needs into a gitignored local directory, and
# verify the bundle renders the whole glyph vocabulary.
#
# Resolves the terminal's fontconfig fallback chain for a configured family
# (`fc-match -s`), truncated at the last font that actually renders something
# in the glyph set, copies those files into <out>/ as NNN-<family>-<name>
# (the NNN prefix preserves fallback order), then runs
# `floating_square_debug cover` over the copy to report any glyph with no
# renderer.
#
# Usage:
#   scripts/collect-fonts.sh                       # game vocabulary -> local-fonts/
#   scripts/collect-fonts.sh ► ▲ U+1F880           # also check extra symbols
#   scripts/collect-fonts.sh --glyphs-file syms.txt
#   scripts/collect-fonts.sh --chain=full          # copy the whole chain
#   scripts/collect-fonts.sh --chain=used          # only fonts that render a glyph
#   scripts/collect-fonts.sh --dry-run
set -euo pipefail

cd "$(dirname "${BASH_SOURCE[0]}")/.."

DEFAULT_FAMILY='CaskaydiaMono Nerd Font'

family="$DEFAULT_FAMILY"
out='local-fonts'
chain_mode='needed'
glyphs_file=""
dry_run=false
clean=false
include_vocabulary=true
glyph_args=()
while [[ $# -gt 0 ]]; do
    case "$1" in
        --family)
            [[ $# -ge 2 ]] || { echo "--family needs a value" >&2; exit 2; }
            family="$2"; shift 2 ;;
        --family=*) family="${1#*=}"; shift ;;
        --out)
            [[ $# -ge 2 ]] || { echo "--out needs a value" >&2; exit 2; }
            out="$2"; shift 2 ;;
        --out=*) out="${1#*=}"; shift ;;
        --chain)
            [[ $# -ge 2 ]] || { echo "--chain needs a value" >&2; exit 2; }
            chain_mode="$2"; shift 2 ;;
        --chain=*) chain_mode="${1#*=}"; shift ;;
        --glyphs-file)
            [[ $# -ge 2 ]] || { echo "--glyphs-file needs a path" >&2; exit 2; }
            glyphs_file="$2"; shift 2 ;;
        --glyphs-file=*) glyphs_file="${1#*=}"; shift ;;
        --dry-run) dry_run=true; shift ;;
        --clean) clean=true; shift ;;
        --no-vocabulary) include_vocabulary=false; shift ;;
        --) shift; glyph_args+=("$@"); break ;;
        -h|--help)
            cat <<'EOF'
usage: scripts/collect-fonts.sh [options] [chars...] [U+XXXX...]
  --family NAME       configured font family (default: CaskaydiaMono Nerd Font)
  --out DIR           destination (default: local-fonts, gitignored)
  --chain MODE        needed (default) | full | used
  --glyphs-file FILE  extra symbols, one per line (chars or U+XXXX), # comments
  --no-vocabulary     only the given symbols, skip the game vocabulary
  --dry-run           report what would be copied, copy nothing
  --clean             empty the destination first
  glyph args          extra symbols to include alongside the game vocabulary
EOF
            exit 0 ;;
        *) glyph_args+=("$1"); shift ;;
    esac
done

case "$chain_mode" in
    needed|full|used) ;;
    *) echo "--chain must be needed|full|used (got '$chain_mode')" >&2; exit 2 ;;
esac

if ! command -v fc-match >/dev/null 2>&1; then
    echo "collect-fonts: fc-match (fontconfig) is required to resolve the chain" >&2
    exit 1
fi

vocabulary() {
    cargo run -q -p game --bin glyph_vocabulary --features debug-tools -- "$@" 2>/dev/null
}

# --- Build the glyph list: "U+XXXX <char>" lines, deduped by codepoint -------
glyph_lines=""
if $include_vocabulary; then
    glyph_lines=$(vocabulary)
fi
if (( ${#glyph_args[@]} > 0 )); then
    glyph_lines+=$'\n'"$(vocabulary "${glyph_args[@]}")"
fi
if [[ -n "$glyphs_file" ]]; then
    file_args=()
    while IFS= read -r line; do
        line="${line%%#*}"
        line="${line#"${line%%[![:space:]]*}"}"
        line="${line%"${line##*[![:space:]]}"}"
        [[ -z "$line" ]] && continue
        file_args+=("$line")
    done < "$glyphs_file"
    if (( ${#file_args[@]} > 0 )); then
        glyph_lines+=$'\n'"$(vocabulary "${file_args[@]}")"
    fi
fi
glyph_lines=$(printf '%s\n' "$glyph_lines" | awk 'NF && !seen[$1]++')
glyph_count=$(printf '%s\n' "$glyph_lines" | grep -c .)
if (( glyph_count == 0 )); then
    echo "collect-fonts: no glyphs to collect (drop --no-vocabulary or pass symbols)" >&2
    exit 2
fi

# --- Resolve the ordered fallback chain --------------------------------------
declare -a chain_file=() chain_fam=() chain_style=()
declare -A file_index=()
while IFS=$'\t' read -r f fam style; do
    [[ -z "$f" ]] && continue
    file_index["$f"]=${#chain_file[@]}
    chain_file+=("$f")
    chain_fam+=("$fam")
    chain_style+=("$style")
done < <(fc-match -s -f '%{file}\t%{family}\t%{style}\n' "$family")
chain_len=${#chain_file[@]}
if (( chain_len == 0 )); then
    echo "collect-fonts: fontconfig returned no fonts for '$family'" >&2
    exit 1
fi

# --- First pick per glyph -> chain index; truncate at the last needed font ---
max_needed=-1
declare -A seen_extra=()
extra_picks=()
while IFS= read -r line; do
    [[ -z "$line" ]] && continue
    cp="${line%% *}"; hex="${cp#U+}"
    pick=$(fc-match -f '%{file}' "$family:charset=${hex}" 2>/dev/null || true)
    [[ -z "$pick" ]] && continue
    if [[ -n "${file_index["$pick"]+x}" ]]; then
        idx="${file_index["$pick"]}"
        (( idx > max_needed )) && max_needed=$idx
    elif [[ -z "${seen_extra["$pick"]+x}" ]]; then
        seen_extra["$pick"]=1
        extra_picks+=("$pick")
    fi
done <<< "$glyph_lines"

# --- Decide which (index, file) pairs to copy --------------------------------
declare -a copy_idx=() copy_file=()
case "$chain_mode" in
    needed)
        cut=$((max_needed + 1))
        for (( i=0; i<cut; i++ )); do copy_idx+=("$i"); copy_file+=("${chain_file[$i]}"); done
        ;;
    full)
        for (( i=0; i<chain_len; i++ )); do copy_idx+=("$i"); copy_file+=("${chain_file[$i]}"); done
        ;;
    used)
        declare -A used_seen=()
        while IFS= read -r line; do
            [[ -z "$line" ]] && continue
            hex="${line%% *}"; hex="${hex#U+}"
            pick=$(fc-match -f '%{file}' "$family:charset=${hex}" 2>/dev/null || true)
            [[ -z "$pick" ]] && continue
            [[ -n "${used_seen["$pick"]+x}" ]] && continue
            used_seen["$pick"]=1
            idx="${file_index["$pick"]:-$chain_len}"
            copy_idx+=("$idx"); copy_file+=("$pick")
        done <<< "$glyph_lines"
        ;;
esac
for pick in "${extra_picks[@]+"${extra_picks[@]}"}"; do
    [[ -n "${file_index["$pick"]+x}" ]] && continue
    copy_idx+=("$chain_len"); copy_file+=("$pick")
done

# --- Copy, deduped by content ------------------------------------------------
mkdir -p "$out"
if $clean && [[ -n "$(ls -A "$out" 2>/dev/null)" ]]; then
    case "$out" in
        ""|"/"|"."|"..")
            echo "collect-fonts: refusing to --clean unsafe path '$out'" >&2
            exit 2 ;;
    esac
    if $dry_run; then
        echo "would clean $out"
    else
        find "$out" -mindepth 1 -maxdepth 1 -exec rm -rf {} +
    fi
fi

declare -A hash_to_name=()
manifest="$out/MANIFEST.tsv"
if ! $dry_run; then
    printf 'index\tfamily\tstyle\tsource\tcopied\n' > "$manifest"
fi
copied=0
skipped=0
for (( k=0; k<${#copy_file[@]}; k++ )); do
    idx="${copy_idx[$k]}"
    f="${copy_file[$k]}"
    [[ -f "$f" ]] || continue
    fam_safe=$(printf '%s' "${chain_fam[$idx]:-unknown}" | tr -c 'A-Za-z0-9._-' '_')
    base=$(basename "$f")
    dest="$(printf '%03d' "$idx")-${fam_safe}-${base}"
    hash=$(sha1sum "$f" 2>/dev/null | awk '{print $1}')
    [[ -z "$hash" ]] && hash="$f"
    if [[ -n "${hash_to_name["$hash"]+x}" ]]; then
        skipped=$((skipped + 1))
        if ! $dry_run; then
            printf '%s\t%s\t%s\t%s\t%s\n' "$idx" "${chain_fam[$idx]:-unknown}" "${chain_style[$idx]:-}" "$f" "${hash_to_name["$hash"]}" >> "$manifest"
        fi
        continue
    fi
    hash_to_name["$hash"]="$dest"
    copied=$((copied + 1))
    if $dry_run; then
        echo "would copy  $f  ->  $out/$dest"
    else
        cp "$f" "$out/$dest"
        printf '%s\t%s\t%s\t%s\t%s\n' "$idx" "${chain_fam[$idx]:-unknown}" "${chain_style[$idx]:-}" "$f" "$dest" >> "$manifest"
    fi
done

echo
echo "family: $family"
echo "chain: $chain_len fonts; $chain_mode mode -> $copied copied ($skipped duplicate)"

if $dry_run; then
    echo "dry run: nothing copied, coverage not checked"
    exit 0
fi

# --- Verify the local bundle renders every glyph -----------------------------
cps=()
while IFS= read -r line; do
    [[ -n "$line" ]] && cps+=("${line%% *}")
done <<< "$glyph_lines"
if (( ${#cps[@]} > 0 )); then
    set +e
    cargo run -q -p floating_square_debug -- cover --dir "$out" "${cps[@]}" \
        > "$out/GLYPH-FONTS.tsv"
    cover_status=$?
    set -e
    missing=$(grep -c $'\tMISSING\t' "$out/GLYPH-FONTS.tsv" || true)
    echo "wrote $out/MANIFEST.tsv and $out/GLYPH-FONTS.tsv"
    echo "coverage: $((glyph_count - missing))/$glyph_count glyphs; $missing missing"
    if (( missing > 0 )); then
        echo "MISSING renders (no font in the bundle has them):"
        awk -F'\t' '$3=="MISSING"{printf "  %s %s\n", $1, $2}' "$out/GLYPH-FONTS.tsv"
        exit 1
    fi
    if (( cover_status != 0 )); then
        echo "cover mode reported an error (status $cover_status)" >&2
        exit "$cover_status"
    fi
fi
