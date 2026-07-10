---
name: code-fix-hygiene
description: Scan files in the working diff for typos, naming conventions, comment hygiene, and editing/coding artifacts across their full contents, and apply decisive cleanup fixes.
---

Check working files and their diffs for hygiene issues by scanning entire files line by line and applying aggressive, surface-level fixes.

## Process

### Step 1 - Identify Context

- Run `jj diff -s` to get a high-level overview of changed files.
- Run `jj diff` (or `jj diff -- <paths>`) to view the full diff content.
- Identify target files specified by the user or modified in the diff.

### Step 2 - Scout for Issues

Read each target file in full. Do not use grep as the primary scanning method, and do not limit analysis to diff hunks. Use the diff as an entry point, but scan the entire file line by line.

Spawn the `scout` subagent:

```
Read these files in full, scanning their entire contents line by line:
{files}

- Scan the complete content of each file for code hygiene issues based on the sections below.
- Do not rely on grep or analyze only the changed hunks.

## Comment scan

- Read every comment (including docstrings, block comments, inline comments, and section headers).
- Evaluate each against the commenting hygiene rules.
- Apply the same scrutiny to both new and existing comments; do not be lenient with either.

### Issues to flag

Flag all code hygiene issues described in the sections below.

#### Editing artifacts

- Spelling typos, grammatical errors, and naming or formatting convention violations.

- Unintended edits, including whitespace noise, newline noise, and merge artifacts.

  Example:
  - An edit or nearby code uses two newlines between sections where existing code uses one.
  - A missing newline at the end of a file.

#### Commenting hygiene

- **Core principle**: Comments must capture *why not*:
  - The non-obvious rationale, rejected alternative, or constraint explaining why the obvious approach was avoided.
  - Any comment describing what the code does or how it works is a violation.

- Inline decorated comments.

  Exception:
  - Multi-line section borders.

  Example:
  - `// --- Title ---` -> replace with `// Title`
  - `# ==== Title ====` -> replace with `# Title`
  - `/* <emoji> Title */` -> replace with `/* Title */`

- Comments explaining how the code works (How) or what it does (What) instead of capturing *why not* (the non-obvious constraint, rejected alternative, or reason the obvious approach was avoided).

  Exception:
  - Section headers that improve readability.
  - Extremely non-obvious code.

  Example:
  - "a = 1+1; /* assign 1+1 to a */"

- Commented-out code and orphaned TODOs.

- Transitional or legacy comments that are not active TODOs.

  Example:
  - "Replaces the old x system"
  - "Migration from y" (where the referenced component no longer exists)

- Comments referencing code, symbols, or constructs that no longer exist in the codebase.

  Example:
  - "// replaced the old foo() call" (where `foo()` is gone and the comment only describes what was removed)
  - "// was previously bar, now baz" (where `bar` no longer appears anywhere)

- Comments describing what the code no longer does, or referencing deleted code or constructs.

  Example:
  - "// no longer do Y" (when describing removed behavior)
  - "// used to call bar()" (where `bar()` is gone)
  Since these comments describe absence rather than active code behavior, remove them.

- Comments restating the size or count of an immediately following list or structure.

  Example:
  - "// All 32 units" before a list of 32 items
  - "# 3 steps:" before a three-item list
  Since the count is obvious from the code, remove these.

- Section-header comments labeling obvious blocks where the code's purpose is already clear.

  Exception:
  - Section headers that improve readability in long files.

  Example:
  - `// Helper functions`
  - `// Main logic`
  - `// Configuration`
  - `// Imports`

- Step-by-step narration of current, subsequent, or future actions, describing what is happening instead of why.

  Example:
  - `// Here we initialize the counter`
  - `// Now let's check if x is valid`
  - `// In this section, we will...`
  - `// First, we...`

- Block-end markers.

  Example:
  - `} // end of for loop`
  - `} // end if`
  - `# end of function`

- "Note:", "Important:", or "Note that:" prefixes that add no informational value.

  Example:
  - `// Note: this returns a string` (when the signature already indicates `-> str`)

- Comments restating type signatures in prose.

  Example:
  - `// returns a string` or `// x is an integer` next to a typed declaration

- Comments explaining language built-ins or the standard library.

  Example:
  - `// map() applies the function to each element`
  - `// filter keeps only items matching the predicate`

- "This function does X" comments that merely repeat the function name.

  Example:
  - `def validate_input():  # This function validates the input`

- Overly detailed docstrings on trivial, single-line functions.

  Example:
  - A one-line `is_even(n): return n % 2 == 0` with a six-line docstring

- Filler words that add no informational value.

  Example:
  - Words like "simply", "just", "basically", or "essentially" (e.g., `// simply iterate and collect results`)

- Conversational or editorial references.

  Example:
  - `// As discussed above`
  - `// As we can see`
  - `// As mentioned earlier`

- Comments narrating the editing process or referencing conversations or interactions rather than the code itself.

  Example:
  - "// replaced x with y" (after a request to replace x with y)
  - "// removed the previous loop"
  - "// updated to use new API as discussed"
  Since these comments describe change history rather than the current code, remove them.

#### Coding artifacts

- Ad-hoc debug statements, such as `print()` or `console.log()`.

  Example:
  - `print("here")`

- Out-of-scope changes.

- Inconsistent coding conventions.

## Output

- List each issue by `file:line` with a brief explanation of why it was flagged.
- Provide a comprehensive and thorough report; do not omit minor issues.
```

### Step 3 - Synthesize Findings

- Be aggressive in trimming and fixing identified issues; prioritize fixing over deferring to the user.
- Defer only when a change carries a genuine risk of breaking behavior, logic, or semantic correctness.
- Verify potential typos against dictionaries or project glossaries before assuming they are incorrect.

### Step 4 - Apply Fixes

```
Apply these hygiene fixes across the full files:
{files}

For the following:
{prioritized fixes}

Do not change behavior, logic, or structure.
Apply one logical fix per edit.
```

### Step 5 - Stop at Diminishing Returns

- Stop if further changes would alter behavior or semantics, or require broader architectural context.

### Step 6 - Verify

- Run `jj diff` to ensure only intended changes were made.
- Run the project's formatter, linter, or tests, if available, to verify correctness.

### Step 7 - Report

Report the following to the user:

1. **Scope**: Analyzed files and a summary of the full-file scan.
2. **Issues Found**: Categorized issues (typos, comments, edits) with `file:line` references.
3. **Fixes Applied**: Description and location of each fix.
4. **Deferred or Skipped**: Genuinely high-risk issues only, including the rationale for deferring them.
5. **Verification**: Results of formatting, linter, or test commands.

## Guardrails

- Never change behavior, logic, or structure; focus exclusively on surface-level hygiene.
- Do not rewrite comments; only remove "what" comments and fix obvious typos.
- Scan and clean the entire content of specified files thoroughly, not just modified diff hunks.
- Remove transitional, legacy, and conversation-narrating comments (such as change-history notes like "replaced x with y") unless they are active TODOs.
- Clean up hygiene issues aggressively and decisively; defer to the user only on genuinely high-risk or behavior-changing changes.