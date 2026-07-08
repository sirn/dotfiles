---
name: code-fix-hygiene
description: Scan files in the working diff for typos, naming conventions, comment hygiene, and editing/coding artifacts across their full contents, and apply decisive cleanup fixes.
---

Check working files and their diffs for hygiene issues, scanning full files line-by-line and applying aggressive surface-level fixes.

## Process

### Step 1 - Identify Context

- Run `jj diff -s` for a high-level overview of changed files.
- Run `jj diff` (or `jj diff -- <paths>`) for full diff content.
- Identify the target files specified by the user or modified in the diff.

### Step 2 - Scout for Issues

For every target file, read the entire file content in full. Do not use grep as the primary scanning method, and do not limit analysis to diff hunks. Use the diff as an entry point, but scan the full file line-by-line.

Spawn the `scout` subagent:

```
Read these files in full, scanning their entire contents line-by-line:
{files}

Do not rely on grep or analyze only the changed hunks. Perform a full-file scan for code hygiene issues across the complete file content according to the sections below.

### Issues to flag

Flag all code hygiene issues according to the sections below.

#### Editing artifacts

- Spelling typos, grammatical errors, and naming/formatting convention violations

- Unintended edits such as whitespace noises, newline noises, merge artifacts

  Example:
  - When existing code uses one newline between sections, but an edit or nearby code uses two
  - Lack of newlines at the end of file

#### Commenting hygiene

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

- Comments that reference code, symbols, or constructs that no longer exist in the codebase

  Example:
  - "// replaced the old foo() call" when foo() is gone and the comment only describes what was removed
  - "// was previously bar, now baz" where bar no longer appears anywhere

- Comments that describe what the code no longer does, or reference code/constructs that no longer exist

  Example:
  - "// no longer do Y" when describing removed behavior
  - "// used to call bar()" where bar() is gone
  These describe absence rather than what the code does—remove them.

- Comments that restate the size or count of the following list/structure

  Example:
  - "// All 32 units" before a list of 32 items
  - "# 3 steps:" before a three-item list
  The count is obvious from the code—remove these.

- Section-header comments that label obvious blocks (the code already says what it is)

  Exception:
  - Section headers that aid readability in long files

  Example:
  - `// Helper functions`
  - `// Main logic`
  - `// Configuration`
  - `// Imports`

- Narration of the current/next/future action step-by-step (describes what's happening instead of why)

  Example:
  - `// Here we initialize the counter`
  - `// Now let's check if x is valid`
  - `// In this section, we will...`
  - `// First, we...`

- Block-end markers (pure noise)

  Example:
  - `} // end of for loop`
  - `} // end if`
  - `# end of function`

- "Note:" / "Important:" / "Note that:" prefixes with no added information

  Example:
  - `// Note: this returns a string` when the signature already says `-> str`

- Comments restating the type signature in prose

  Example:
  - `// returns a string` or `// x is an integer` next to a typed declaration

- Comments explaining language builtins/standard library

  Example:
  - `// map() applies the function to each element`
  - `// filter keeps only items matching the predicate`

- "This function does X" comments that just repeat the function name

  Example:
  - `def validate_input():  # This function validates the input`

- Over-detailed docstrings on trivial one-liner functions

  Example:
  - A one-line `is_even(n): return n % 2 == 0` with a 6-line docstring

- Filler words adding no information

  Example:
  - Use of "simply", "just", "basically", "essentially" (e.g., `// simply iterate and collect results`)

- Conversational/editorial references (reads like an essay, not code)

  Example:
  - `// As discussed above`
  - `// As we can see`
  - `// As mentioned earlier`

- Comments that narrate the editing process or refer to our conversation/interaction rather than the code itself

  Example:
  - "// replaced x with y" after the user asked to replace x with y
  - "// removed the previous loop"
  - "// updated to use new API as discussed"
  These comments describe the change history, not the code—remove them.

#### Coding artifacts

- Adhoc debug `print()`/`console.log()`/etc.

  Example:
  - `print("here")`

- Out-of-scope changes

- Inconsistent coding conventions

## Output

Report a list of `file:line` for each issue with a brief explanation why it was flagged.
```

### Step 3 - Synthesize Findings

- Take an aggressive stance on trimming and fixing identified issues. Prioritize fixing over deferring to the user.
- Only defer findings when a change carries a genuine risk of breaking behavior, logic, or semantic correctness.
- Verify typos against dictionaries or project glossary terms before assuming.

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

- Stop only if further changes would alter behavior/semantics, or require broader architectural context.

### Step 6 - Verify

- Run `jj diff` to ensure only intended changes were made.
- Run project formatter, linter, or tests if available to verify correctness.

### Step 7 - Report

Report to the user:

1. **Scope**: Analyzed files and full-file scan summary.
2. **Issues Found**: Categorized issues (typos, comments, edits) with `file:line` references.
3. **Fixes Applied**: Description and location of each fix.
4. **Deferred/Skipped**: Only genuinely high-risk issues, with rationale for why they were deferred.
5. **Verification**: Results of formatting, linting, or test commands.

## Guardrails

- Never change behavior, logic, or structure—only address surface-level hygiene.
- Do not rewrite comments; only remove "what"-comments and fix obvious typos.
- Perform a thorough full-file scan and cleanup across the entire content of specified files, not just within the modified diff hunks.
- Remove transitional, legacy, and conversation-narrating comments (change-history notes like "replaced x with y") unless they are contextual TODOs.
- Clean up hygiene issues aggressively and decisively. Only defer to the user on genuinely high-risk or behavior-changing cases.