#!/usr/bin/env bash
# check-commit-msg.sh — Verify commit message lines stay within a character limit.
#
# Usage:
#   check-commit-msg.sh [OPTIONS] [REV]
#
# Options:
#   -m, --max-width N   Maximum line width (default: 72)
#   -v, --vcs VCS       Force VCS: 'jj' or 'git' (auto-detected if omitted)
#   -h, --help          Show this help message
#
# REV:
#   Revision or commit reference (default: current head — '@' for jj, 'HEAD' for git).
#
# Exit codes:
#   0  All lines within limit.
#   1  One or more lines exceed the limit.
#   2  Usage or runtime error.

set -euo pipefail

MAX_WIDTH=72
FORCE_VCS=""
REV=""

usage() {
  sed -n '2,/^$/s/^# //p' "$0"
}

# --- Parse arguments ---
while [[ $# -gt 0 ]]; do
  case "$1" in
  -m | --max-width)
    MAX_WIDTH="$2"
    shift 2
    ;;
  -v | --vcs)
    FORCE_VCS="$2"
    shift 2
    ;;
  -h | --help)
    usage
    exit 0
    ;;
  --)
    shift
    REV="${1:-}"
    break
    ;;
  -*)
    echo "Unknown option: $1" >&2
    exit 2
    ;;
  *)
    REV="$1"
    shift
    ;;
  esac
done

# --- Detect VCS ---
detect_vcs() {
  if [[ -n $FORCE_VCS ]]; then
    echo "$FORCE_VCS"
    return
  fi
  if command -v jj &>/dev/null && jj root &>/dev/null 2>&1; then
    echo "jj"
  elif command -v git &>/dev/null && git rev-parse --is-inside-work-tree &>/dev/null 2>&1; then
    echo "git"
  else
    echo "Error: No supported VCS found (jj or git)" >&2
    exit 2
  fi
}

VCS=$(detect_vcs)

# --- Get commit message ---
get_message_jj() {
  local rev="${1:-@}"
  jj log -r "$rev" -n 1 --no-graph -T 'description' 2>/dev/null || {
    echo "Error: Failed to get commit message for '$rev' via jj" >&2
    exit 2
  }
}

get_message_git() {
  local rev="${1:-HEAD}"
  git log -1 --format='%B' "$rev" 2>/dev/null | sed '/^$/d' || {
    echo "Error: Failed to get commit message for '$rev' via git" >&2
    exit 2
  }
}

# Default rev
if [[ -z $REV ]]; then
  if [[ $VCS == "git" ]]; then
    REV="HEAD"
  else
    REV="@"
  fi
fi

# Fetch message
MSG=$({
  if [[ $VCS == "jj" ]]; then
    get_message_jj "$REV"
  else
    get_message_git "$REV"
  fi
})

if [[ -z $MSG ]]; then
  echo "Warning: Empty commit message for '$REV'" >&2
  exit 0
fi

# --- Check line lengths ---
FAIL=0
LINENO_COUNT=0

while IFS= read -r line; do
  LINENO_COUNT=$((LINENO_COUNT + 1))
  LEN=${#line}
  if ((LEN > MAX_WIDTH)); then
    echo "LINE TOO LONG ($LEN/$MAX_WIDTH) [line $LINENO_COUNT]: $line"
    FAIL=1
  fi
done <<<"$MSG"

if ((FAIL)); then
  echo ""
  echo "FAIL: $LINENO_COUNT line(s) checked; one or more exceed $MAX_WIDTH characters."
  exit 1
fi

echo "OK: All $LINENO_COUNT line(s) within $MAX_WIDTH characters."
exit 0
