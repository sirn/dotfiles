#!/bin/bash
# RTK auto-rewrite hook for Claude Code PreToolUse:Bash
set -euo pipefail

INPUT=$(cat)
CMD=$(echo "$INPUT" | jq -r '.tool_input.command // empty')

if [ -z "$CMD" ]; then
  exit 0
fi

case "$CMD" in
  *'<<'*) exit 0 ;;
esac

REWRITTEN=$(rtk rewrite "$CMD" 2>/dev/null) || exit 0

if [ "$CMD" = "$REWRITTEN" ]; then
  exit 0
fi

echo "$INPUT" | jq -c --arg cmd "$REWRITTEN" \
  '{hookSpecificOutput: {
     hookEventName: "PreToolUse",
     permissionDecision: "allow",
     permissionDecisionReason: "RTK auto-rewrite (token optimization)",
     updatedInput: (.tool_input | .command = $cmd)
   }}'
