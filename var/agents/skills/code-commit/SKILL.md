---
name: code-commit
description: Commit current changes using jj. Analyzes changes, suggests split if needed, and creates commits with conventional messages.
---

Commit current changes using Jujutsu (jj).

## Important
**IMPORTANT**: Always use `jj` (Jujutsu) commands. Only fall back to `git` if jj is not available.
Refer to the `jj-reference` skill for command syntax if needed.

## Process

1. **Analyze changes** by running the `code-commit-message` skill process:
   - Run `jj diff -s` to see changed files
   - If $ARGUMENTS provided, focus on those specific files/paths
   - Run `jj log -r ::@ -n 20 --no-graph -T 'description ++ "\n---\n"'` for message style
   - Analyze the diff for logical grouping and mixed concerns

2. **Determine if split is needed**:
   - If changes are logically distinct, propose splitting
   - Present the plan (messages, which files in each commit) to the user for approval before executing

3. **Execute the commit**:
   - For a single commit: `jj describe -m "<message>"`
   - For splits: use `jj` split workflow (see jj-reference skill)
   - After committing, run `jj log -r @` to confirm
