#!/usr/bin/env bash
# Notify Codex - send desktop notification when agent finishes a turn.

if [ -p /dev/stdin ]; then
  INPUT=$(cat)
else
  INPUT="$1"
fi

TYPE=$(echo "$INPUT" | jq -r '.type // empty' 2>/dev/null)
if [ "$TYPE" = "agent-turn-complete" ]; then
  last_msg=$(echo "$INPUT" | jq -r '."last-assistant-message" // empty' 2>/dev/null | tr '\n' ' ' | head -c 200)
  body="${last_msg:-Codex has finished their turn}"
  toastify send "Codex" "$body"
fi
