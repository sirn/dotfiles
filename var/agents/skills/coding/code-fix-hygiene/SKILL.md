---
name: code-fix-hygiene
description: Check diff for typos, conventions, comment quality, and unintended edits. Use when asked to clean up a diff, review changes for consistency, or perform pre-commit hygiene.
---

Check the working diff for hygiene issues and apply minimal fixes.

## Process

### Step 1 - Identify Context

- Run `jj diff -s` to see changed files and get a high-level overview.
- Run `jj diff` (or `jj diff -- <paths>`) for full diff content.
- If the user specified files or paths, restrict analysis to those.

### Step 2 - Scout for Issues

Spawn `scout` subagent:

```
Analyze this diff in the following files:
{files}

## Issues to flag

Flag all code hygiene issues according to the sections below.

### Editing artifacts

- Spelling typos, grammatical errors, and naming/formatting convention violations

- Unintended edits such as whitespace noises, newline noises, merge artifacts

  Example:
  - When an existing code uses one newline between section, but the edit uses two
  - Lack of newlines at the end of file

### Commenting hygiene

- Inline decorated comments

  Exception:
  - Multi-line section borders

  Example:
  - `// --- Title ---` -> replace with `// Title`
  - `# ==== Title ====` -> replace with `# Title`
  - `/* <emoji> Title */` -> replace with `/* Title */`

- Comments that explain _what the code_ does rather than _why_

  Exception:
  - Section headers for readability
  - The code is non-obvious

  Example:
  - "a = 1+1; /* assign 1+1 to a */"

- Commented-out code, orphaned TODOs

- Transitional or legacy comments that are not TODOs

  Example:
  - "Replaces the old x system"
  - "Migration from y" where the referenced component no longer exists

### Coding artifact

- Adhoc debug `print()`/`console.log()`/etc.

  Example:
  - `print("here")`

- Out-of-scope changes

- Inconsistent coding conventions

## Output

Report list of `file:line` for each issue with a brief explaination why it was flagged.
```

### Step 3 - Synthesize Findings

- Filter to clear, actionable issues with low risk of false positives.
- Flag ambiguous findings for user confirmation rather than fixing speculatively.
- For typos: prefer dictionary lookups or project glossary terms before assuming.

### Step 4 - Apply Fixes

```
Apply these hygiene fixes in the following files:
{files}

For the following:
{prioritized fixes}

Do not change behavior, logic, or structure.
Apply one logical fix per edit.
```

### Step 5 - Stop at Diminishing Returns

- Stop when remaining findings are speculative or stylistic preferences.
- Stop when fixes would alter behavior or semantics.
- Stop when fixes require broader context beyond the diff.

### Step 6 - Verify

- Run `jj diff` to confirm only intended changes were made and no side effects introduced.
- Run the project formatter if applicable.
- If a test/lint command is available, run it to confirm nothing broke.

### Step 7 - Report

Report the following to the user:

1. **Scope**: files analyzed and diff summary
2. **Issues Found**: categorized by type (typos, comment quality, unintended edits) with file:line references
3. **Fixes Applied**: each fix with location and description
4. **Deferred/Skipped**: issues identified but not fixed, with rationale
5. **Verification**: result of formatter or check commands

## Guardrails

- Never change behavior, logic, or structure — only surface-level hygiene.
- Do not rewrite comments or improve their content; only remove _what_-comments and fix obvious typos.
- Do not fix issues outside the diff scope unless the user explicitly asked for broader cleanup.
- Preserve intentional debug logging, TODOs with context, and commented-out code that has a clear purpose.
- Only acceptable transitional/legacy comments are TODOs with context — all others should be removed.
- If a finding is ambiguous or could be intentional, defer to the user rather than guessing.
