#!/usr/bin/env bash
#
# Typecheck support for the vendored pi extensions.
#
# The extensions import @earendil-works/* and typebox from the installed pi
# package, which lives in the Nix store at a versioned path. We resolve it at
# runtime (the compiled `pi` binary references it) and symlink a gitignored
# node_modules/ here. That wiring is shared by two consumers:
#
#   - LSP: `--setup` links node_modules/ so typescript-language-server (and any
#     editor) can resolve imports for hover/definition/references. The `lsp`
#     skill CLI exposes navigation but not diagnostics; for error detection use
#     the gate below or an editor's live diagnostics.
#   - Gate: a baseline-regression `tsc --noEmit` across all extensions. It
#     fails only on *new* errors; fixing a baseline error is an improvement
#     (exit 0), not a failure. Re-baseline with --update-baseline.
#
# Usage:
#   ./check-extensions.sh                    # link deps + check against baseline
#   ./check-extensions.sh --setup            # link deps only (for LSP), then exit
#   ./check-extensions.sh --update-baseline  # link deps + rewrite the baseline
set -euo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$HERE"

BASELINE="$HERE/extensions.tsc-baseline"
NM="$HERE/node_modules"
current_file=""
trap '[[ -n ${current_file:-} ]] && rm -f "$current_file"' EXIT

# Resolve a possibly-symlinked path to its final target without `readlink -f`,
# which BSD readlink (macOS) does not support.
resolve_path() {
  local p="$1" t
  while t="$(readlink "$p" 2>/dev/null)"; do
    case "$t" in
    /*) p="$t" ;;
    *) p="$(dirname "$p")/$t" ;;
    esac
  done
  printf '%s\n' "$p"
}

resolve_pi_pkg() {
  local pibin
  pibin="$(command -v pi 2>/dev/null || true)"
  if [[ -z $pibin ]]; then
    echo "pi not found on PATH; cannot resolve the pi package" >&2
    return 1
  fi
  pibin="$(resolve_path "$pibin")"
  # The compiled `pi` binary references the pi-<version> store path carrying
  # lib/node_modules/@earendil-works/*.
  local cand
  while IFS= read -r cand; do
    if [[ -d "$cand/lib/node_modules/@earendil-works/pi-coding-agent" ]]; then
      echo "$cand"
      return 0
    fi
  done < <(nix-store -q --references "$pibin" 2>/dev/null | grep -E '/nix/store/.*-pi-[0-9]')
  echo "could not locate the pi package (lib/node_modules/@earendil-works/pi-coding-agent)" >&2
  return 1
}

link_node_modules() {
  local pipkg="$1"
  local pcac="$pipkg/lib/node_modules/@earendil-works/pi-coding-agent"
  local ea="$pcac/node_modules/@earendil-works"
  local missing=()
  [[ -d $pcac ]] || missing+=("$pcac")
  [[ -d "$ea/pi-ai" ]] || missing+=("$ea/pi-ai")
  [[ -d "$ea/pi-agent-core" ]] || missing+=("$ea/pi-agent-core")
  [[ -d "$ea/pi-tui" ]] || missing+=("$ea/pi-tui")
  [[ -d "$pcac/node_modules/typebox" ]] || missing+=("$pcac/node_modules/typebox")
  if ((${#missing[@]})); then
    echo "missing pi package dirs: ${missing[*]}" >&2
    return 1
  fi
  mkdir -p "$NM/@earendil-works"
  ln -sfn "$pcac" "$NM/@earendil-works/pi-coding-agent"
  ln -sfn "$ea/pi-ai" "$NM/@earendil-works/pi-ai"
  ln -sfn "$ea/pi-agent-core" "$NM/@earendil-works/pi-agent-core"
  ln -sfn "$ea/pi-tui" "$NM/@earendil-works/pi-tui"
  ln -sfn "$pcac/node_modules/typebox" "$NM/typebox"
}

run_tsc() {
  local out ec
  out="$(nix run nixpkgs#typescript -- --noEmit -p tsconfig.json 2>&1)" && ec=0 || ec=$?
  printf '%s\n' "$out"
  return "$ec"
}

# Keep only error diagnostics, drop the (line,col) locator so unrelated line
# shifts don't read as regressions, and sort. LC_ALL=C is required so the sort
# order matches the baseline regardless of the caller's locale.
normalize() {
  LC_ALL=C grep -E 'error TS[0-9]+' |
    LC_ALL=C sed -E 's/\([0-9]+,[0-9]+\)://' |
    LC_ALL=C sort
}

count_lines() {
  local n
  n="$(printf '%s\n' "$1" | grep -cE '.' 2>/dev/null)" || n=0
  printf '%s' "$n"
}

main() {
  local mode="check"
  case "${1:-}" in
  --setup) mode="setup" ;;
  --update-baseline) mode="update" ;;
  "") mode="check" ;;
  *)
    echo "usage: $0 [--setup|--update-baseline]" >&2
    exit 2
    ;;
  esac

  local pipkg
  pipkg="$(resolve_pi_pkg)" || exit 1
  link_node_modules "$pipkg"

  if [[ $mode == "setup" ]]; then
    echo "Linked node_modules/ — LSP and tsc can now resolve @earendil-works/* and typebox."
    exit 0
  fi

  local tsc_out tsc_ec
  tsc_out="$(run_tsc)" && tsc_ec=0 || tsc_ec=$?
  local current
  current="$(printf '%s\n' "$tsc_out" | normalize)" || true

  # Non-zero tsc exit with no type diagnostics means tsc/nix failed to run at
  # all; a genuinely clean run exits 0, and type errors populate $current.
  if ((tsc_ec != 0)) && [[ -z $current ]]; then
    echo "tsc failed to run (no type diagnostics produced); exit $tsc_ec" >&2
    printf '%s\n' "$tsc_out" >&2
    exit 2
  fi

  current_file="$(mktemp)"
  printf '%s\n' "$current" >"$current_file"

  if [[ $mode == "update" ]]; then
    cp "$current_file" "$BASELINE"
    echo "Wrote $(count_lines "$(cat "$BASELINE")") baseline errors to $BASELINE"
    exit 0
  fi

  if [[ ! -f $BASELINE ]]; then
    echo "No baseline at $BASELINE; run with --update-baseline first." >&2
    exit 2
  fi

  local new removed
  new="$(LC_ALL=C comm -23 "$current_file" "$BASELINE")"
  removed="$(LC_ALL=C comm -13 "$current_file" "$BASELINE")"

  local ncur nbase nnew nrem
  ncur="$(count_lines "$current")"
  nbase="$(count_lines "$(cat "$BASELINE")")"
  nnew="$(count_lines "$new")"
  nrem="$(count_lines "$removed")"

  echo "current: $ncur  baseline: $nbase  new: $nnew  fixed: $nrem"
  if [[ -n $new ]]; then
    echo "--- NEW errors (regressions) ---" >&2
    printf '%s\n' "$new" >&2
  fi
  if [[ -n $removed ]]; then
    echo "--- fixed vs baseline (re-baseline with --update-baseline) ---"
    printf '%s\n' "$removed"
  fi

  if [[ -n $new ]]; then
    exit 1
  fi
  echo "OK: no new type errors"
}

main "$@"
