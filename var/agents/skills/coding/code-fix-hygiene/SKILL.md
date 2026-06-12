---
name: code-fix-hygiene
description: Check diff for typos, conventions, comment quality, and unintended edits. Use when asked to clean up a diff, review changes for consistency, or perform pre-commit hygiene.
---

Check the working diff for hygiene issues and apply minimal fixes.

## Process

1. **Identify context**:
   - Run `jj diff -s` to see changed files and get a high-level overview.
   - Run `jj diff` (or `jj diff -- <paths>`) for full diff content.
   - If the user specified files or paths, restrict analysis to those.

2. **Spawn `scout`**:
   ```
   Analyze this diff in the following files:
   {files}

   For all the following:
   - spelling typos, grammatical errors, and naming/formatting convention violations
   - comments that explain *what* the code does rather than *why* (except section headers) — flag for removal, preserving rationale/tradeoff/non-obvious comments
   - unintended edits like debug logging, commented-out code, orphaned TODOs, whitespace noise, merge artifacts, or out-of-scope changes
   - inline decorated comments such as `// --- Title ----------` (multi-line section borders (`// -------------------` are fine)
   - transitional or legacy comments like "Replaces the old x system" or "Migration from y" where the referenced thing no longer exists — only acceptable transitional comments are TODOs

   Report file:line for each issue.
   ```

3. **Synthesize findings**:
   - Filter to clear, actionable issues with low risk of false positives.
   - Flag ambiguous findings for user confirmation rather than fixing speculatively.
   - For typos: prefer dictionary lookups or project glossary terms before assuming.

4. **Delegate to `worker`**:
   ```
   Apply these hygiene fixes in the following files:
   {files}

   For the following:
   {prioritized fixes}

   Remove obvious *what*-comments, fix typos, remove unintended edits, remove stale transitional/legacy comments unless they are TODOs.
   Do not change behavior, logic, or structure.
   Apply one logical fix per edit.
   ```

5. **Stop at diminishing returns**:
   - Stop when remaining findings are speculative or stylistic preferences.
   - Stop when fixes would alter behavior or semantics.
   - Stop when fixes require broader context beyond the diff.

6. **Verify**:
   - Run `jj diff` to confirm only intended changes were made and no side effects introduced.
   - Run the project formatter if applicable.
   - If a test/lint command is available, run it to confirm nothing broke.

## Output

1. **Scope**: files analyzed and diff summary
2. **Issues Found**: categorized by type (typos, comment quality, unintended edits) with file:line references
3. **Fixes Applied**: each fix with location and description
4. **Deferred/Skipped**: issues identified but not fixed, with rationale
5. **Verification**: result of formatter or check commands

## Guardrails

- Never change behavior, logic, or structure — only surface-level hygiene.
- Do not rewrite comments or improve their content; only remove *what*-comments and fix obvious typos.
- Do not fix issues outside the diff scope unless the user explicitly asked for broader cleanup.
- Preserve intentional debug logging, TODOs with context, and commented-out code that has a clear purpose.
- Only acceptable transitional/legacy comments are TODOs with context — all others should be removed.
- If a finding is ambiguous or could be intentional, defer to the user rather than guessing.
