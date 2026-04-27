---
name: code-debug
description: Debug issues by researching errors and proposing minimal fixes. Use when user asks to troubleshoot or debug a failure.
---

Troubleshoot a problem by delegating research to the code-debug-researcher agent.

## Process

1. Identify the error message, log, or failure symptom from the user's request or context
2. Spawn agents:
   - `code-debug-researcher`: "Investigate the error or failure in {context} and propose a minimal fix with sources"
   - `simplicity-reviewer`: "Ensure the proposed fix is minimal and addresses only the root cause without introducing new abstractions. Prioritize deleting or simplifying existing code over adding new logic."

3. Synthesize findings into actionable steps

## Output

1. **Likely cause**
2. **Evidence** (links or doc references)
3. **Fix**
4. **Verify**
