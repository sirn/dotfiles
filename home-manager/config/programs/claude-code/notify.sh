#!/usr/bin/env bash
# Notify Claude Code - send desktop notification when agent finishes a turn.

input=$(cat)
last_msg=$(echo "$input" | jaq -r '.last_assistant_message // empty' 2>/dev/null | tr '\n' ' ' || true)
last_msg="${last_msg:0:200}"

if [ -z "$last_msg" ]; then
  transcript=$(echo "$input" | jaq -r '.transcript_path // empty' 2>/dev/null || true)
  if [ -n "$transcript" ] && [ -f "$transcript" ]; then
    last_msg=$(jaq -sr '
      [.[] | select(.type == "assistant")] | last |
      [.message.content[]? | select(.type == "text") | .text] | join(" ")
    ' "$transcript" 2>/dev/null | tr '\n' ' ' || true)
    last_msg="${last_msg:0:200}"
  fi
fi

body="${last_msg:-Claude Code has finished their turn}"
body="${body#"${body%%[![:space:]]*}"}"
body="${body%"${body##*[![:space:]]}"}"
toastify send "Claude Code" "$body"
