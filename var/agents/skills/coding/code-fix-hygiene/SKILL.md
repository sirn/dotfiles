---
name: code-fix-hygiene
description: Check diff for typos, conventions, comment quality, and unintended edits. Use when asked to clean up a diff, review changes for consistency, or perform pre-commit hygiene.
---

Check the working diff for hygiene issues and apply minimal fixes.

## Process

### Step 1 - Identify Context

- Run `jj diff -s` for a high-level overview of changed files.
- Run `jj diff` (or `jj diff -- <paths>`) for full diff content.
- Restrict analysis to user-specified files or paths.

### Step 2 - Scout for Issues

Spawn the `scout` subagent:

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

- Filter for clear, low-risk, actionable issues.
- Defer ambiguous findings to the user instead of fixing speculatively.
- Verify typos against dictionaries or project glossary terms before assuming.

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

- Stop if remaining findings are speculative or stylistic.
- Stop if fixes would alter behavior/semantics, or require broader context.

### Step 6 - Verify

- Run `jj diff` to ensure only intended changes were made.
- Run project formatter, linter, or tests if available to verify correctness.

### Step 7 - Report

Report to the user:

1. **Scope**: Analyzed files and diff summary.
2. **Issues Found**: Categorized issues (typos, comments, edits) with `file:line` references.
3. **Fixes Applied**: Description and location of each fix.
4. **Deferred/Skipped**: Identified but unfixed issues, with rationale.
5. **Verification**: Results of formatting, linting, or test commands.

## Guardrails

- Never change behavior, logic, or structure—only address surface-level hygiene.
- Do not rewrite comments; only remove "what"-comments and fix obvious typos.
- Restrict fixes to the diff scope unless broader cleanup is explicitly requested.
- Preserve purposeful debug logging, clear TODOs, and intentional commented-out code.
- Remove transitional or legacy comments unless they are contextual TODOs.
- Defer to the user on ambiguous or potentially intentional code.
