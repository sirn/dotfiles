#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils nix cacert
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"

if [ -n "${NIX_SSL_CERT_FILE:-}" ] && [ -f "${NIX_SSL_CERT_FILE}" ]; then
  export SSL_CERT_FILE="${NIX_SSL_CERT_FILE}"
fi

c_blue=$(tput setaf 4 2>/dev/null || true)
c_green=$(tput setaf 2 2>/dev/null || true)
c_yellow=$(tput setaf 3 2>/dev/null || true)
c_red=$(tput setaf 1 2>/dev/null || true)
c_reset=$(tput sgr0 2>/dev/null || true)

log_info() {
  printf '%s[INFO]%s %s\n' "$c_blue" "$c_reset" "$*"
}

log_ok() {
  printf '%s[OK]%s   %s\n' "$c_green" "$c_reset" "$*"
}

log_skip() {
  printf '%s[SKIP]%s %s\n' "$c_yellow" "$c_reset" "$*"
}

log_err() {
  printf '%s[ERR]%s  %s\n' "$c_red" "$c_reset" "$*" >&2
}

# Discover all update.sh scripts under pkgs/by-name/
scripts=()
for f in "$script_dir"/by-name/*/update.sh; do
  [ -f "$f" ] && scripts+=("$f")
done

if [ ${#scripts[@]} -eq 0 ]; then
  log_err "No update.sh scripts found under $script_dir/by-name/"
  exit 1
fi

# Allow filtering by package name: pkgs/update.sh coord repoman
if [ "$#" -gt 0 ]; then
  filtered=()
  for f in "${scripts[@]}"; do
    pkg=$(basename "$(dirname "$f")")
    for arg in "$@"; do
      if [ "$pkg" = "$arg" ]; then
        filtered+=("$f")
        break
      fi
    done
  done
  if [ ${#filtered[@]} -eq 0 ]; then
    log_err "No matching packages for: $*"
    log_info "Available: $(for f in "${scripts[@]}"; do basename "$(dirname "$f")"; done | tr '\n' ' ')"
    exit 1
  fi
  scripts=("${filtered[@]}")
fi

log_info "Running ${#scripts[@]} update script(s)..."

failed=()
for f in "${scripts[@]}"; do
  pkg=$(basename "$(dirname "$f")")
  # Run via the script's shebang (nix-shell) by executing it directly,
  # not via `bash`, so nix-shell packages (nodejs, jq, etc.) are available.
  if "$f" >/tmp/update-$pkg.log 2>&1; then
    log_ok "$pkg"
  else
    # Check if it was just "already at latest" — that's OK
    if grep -q "Already at latest" /tmp/update-$pkg.log 2>/dev/null; then
      log_ok "$pkg (already at latest)"
    else
      log_err "$pkg (see /tmp/update-$pkg.log)"
      cat /tmp/update-$pkg.log >&2
      failed+=("$pkg")
    fi
  fi
  rm -f /tmp/update-$pkg.log
done

if [ ${#failed[@]} -gt 0 ]; then
  log_err "Failed: ${failed[*]}"
  exit 1
fi

log_info "All done."
