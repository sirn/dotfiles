#!/usr/bin/env bash
# Notify Claude Code - send desktop notification when agent finishes a turn.

input=$(cat)
last_msg=$(echo "$input" | jq -r '.last_assistant_message // empty' 2>/dev/null | tr '\n' ' ' | head -c 200)

if [ -z "$last_msg" ]; then
  transcript=$(echo "$input" | jq -r '.transcript_path // empty' 2>/dev/null)
  if [ -n "$transcript" ] && [ -f "$transcript" ]; then
    last_msg=$(jq -sr '
      [.[] | select(.type == "assistant")] | last |
      [.message.content[]? | select(.type == "text") | .text] | join(" ")
    ' "$transcript" 2>/dev/null | tr '\n' ' ' | head -c 200)
  fi
fi

body="${last_msg:-Claude Code has finished their turn}"
toastify send "Claude Code" "$body"
