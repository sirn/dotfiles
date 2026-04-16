#!/usr/bin/env bash
# Notify Codex - send desktop notification when agent finishes a turn.

if [ -p /dev/stdin ]; then
  INPUT=$(cat)
else
  INPUT="$1"
fi

TYPE=$(echo "$INPUT" | jaq -r '.type // empty' 2>/dev/null || true)
if [ "$TYPE" = "agent-turn-complete" ]; then
  last_msg=$(echo "$INPUT" | jaq -r '."last-assistant-message" // empty' 2>/dev/null | tr '\n' ' ' | head -c 200 || true)
  body="${last_msg:-Codex has finished their turn}"
  body="${body#"${body%%[![:space:]]*}"}"
  body="${body%"${body##*[![:space:]]}"}"
  toastify send "Codex" "$body"
fi
