---
name: code-debug
description: Debug issues by researching errors and proposing minimal fixes. Use when user asks to troubleshoot or debug a failure.
---

Troubleshoot a problem by delegating focused research and risk review.

## Process

1. Identify the error message, log, or failure symptom from the user's request or context.
2. Spawn agents:
   - `researcher`:
     ```
     Investigate the error or failure in {context}. Find likely root causes, official docs or known issues, minimal fixes, and verification steps with sources.
     ```
   - `reviewer`:
     ```
     Review the likely fix for {context} with a minimal-fix risk lens. Ensure it addresses only the root cause, avoids new abstractions, and note edge cases or regressions.
     ```
3. Synthesize findings into actionable steps.
4. If fixing is requested, make the smallest change that addresses the likely root cause and verify with the most specific command.

## Stop Condition

- If a proposed fix fails twice, stop, re-evaluate the diagnosis, and ask for guidance.

## Output

1. **Likely cause**
2. **Evidence** (links or doc references)
3. **Fix**
4. **Verify**
