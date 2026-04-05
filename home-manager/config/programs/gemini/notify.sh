#!/usr/bin/env bash
# Notify Gemini CLI - send desktop notification when agent finishes a turn.

input=$(cat)
last_msg=$(echo "$input" | jq -r '.prompt_response // empty' 2>/dev/null | tr '\n' ' ' | head -c 200)
body="${last_msg:-Gemini CLI has finished their turn}"
toastify send "Gemini CLI" "$body"
