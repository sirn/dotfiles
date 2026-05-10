---
name: code-fix-test
description: Diagnose and fix test, lint, or validation failures. Use only when explicitly asked to fix failing tests, lint, formatting, or check commands.
---

Diagnose and fix validation failures with minimal targeted changes.

## Process

1. Identify context:
   - If failure output is provided, read it fully before changing anything.
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified files, commands, or failures, focus on those.

2. Reproduce or inspect the failure:
   - Prefer the exact failing command from the user or project output.
   - If no command is provided, detect the relevant test/lint/check command from instructions, task runners, wrappers, package manager scripts, then common defaults.
   - Use proper timeouts and avoid watch modes.

3. Diagnose root cause:
   - Distinguish product-code bugs, test bugs, environment issues, and stale expectations.
   - Prefer the smallest fix that addresses the real failure.
   - Do not mask failures by weakening assertions, skipping tests, or broadening ignores unless the user explicitly approves and the rationale is sound.

4. Apply a minimal fix and rerun the relevant command.

5. Stop condition:
   - If a fix fails twice, stop, provide root-cause analysis, and ask for guidance.

## Output

1. **Failing Command**
2. **Root Cause**
3. **Fix Applied**
4. **Verification Result**
5. **Remaining Issues** requiring manual action
