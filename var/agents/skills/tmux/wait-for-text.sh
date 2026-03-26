#!/usr/bin/env bash
set -euo pipefail

TARGET=""
PATTERN=""
TIMEOUT=15
INTERVAL=0.5
LINES=1000
FIXED=0

usage() {
  echo "Usage: $0 -t <target> -p <pattern> [-T timeout] [-i interval] [-l lines] [-F]" >&2
}

while getopts ":t:p:T:i:l:Fh" opt; do
  case "$opt" in
  t) TARGET="$OPTARG" ;;
  p) PATTERN="$OPTARG" ;;
  T) TIMEOUT="$OPTARG" ;;
  i) INTERVAL="$OPTARG" ;;
  l) LINES="$OPTARG" ;;
  F) FIXED=1 ;;
  h)
    usage
    exit 0
    ;;
  :)
    echo "Missing value for -$OPTARG" >&2
    usage
    exit 1
    ;;
  \?)
    echo "Unknown option: -$OPTARG" >&2
    usage
    exit 1
    ;;
  esac
done

if [ -z "$TARGET" ] || [ -z "$PATTERN" ]; then
  usage
  exit 1
fi

max_attempts=$(awk "BEGIN { if ($INTERVAL <= 0) { print 1 } else { print int(($TIMEOUT / $INTERVAL) + 0.999999) } }")
if [ "$max_attempts" -lt 1 ]; then
  max_attempts=1
fi

attempt=0
while true; do
  output=$(tmux capture-pane -p -J -t "$TARGET" -S "-$LINES" 2>/dev/null || true)

  if [ "$FIXED" -eq 1 ]; then
    if printf '%s\n' "$output" | grep -qF -- "$PATTERN"; then
      exit 0
    fi
  else
    if printf '%s\n' "$output" | grep -qE -- "$PATTERN"; then
      exit 0
    fi
  fi

  attempt=$((attempt + 1))
  if [ "$attempt" -ge "$max_attempts" ]; then
    echo "Timeout waiting for pattern: $PATTERN" >&2
    echo "Target: $TARGET" >&2
    echo "Last captured output:" >&2
    printf '%s\n' "$output" | tail -n 20 >&2
    exit 1
  fi

  sleep "$INTERVAL"
done
