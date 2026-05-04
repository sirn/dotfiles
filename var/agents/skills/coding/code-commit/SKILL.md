---
name: code-commit
description: Commit current changes using jj. Analyzes changes, suggests commit messages following repository conventions, splits when needed, and creates commits.
---

Commit current changes using Jujutsu (jj).

## Important

**IMPORTANT**: Always use `jj` (Jujutsu) commands. Only fall back to `git` if jj is not available.
Refer to the `jujutsu` skill for command syntax and **Best Practices** (local commit autonomy, revision references, logical grouping, etc.).

## Policy

- You may commit freely — no user approval needed before committing.
- If a commit needs splitting, use `jj op` to identify the current operation, then attempt `jj split -r <id> -m "<msg>" -- <file>`. Only split the current working-copy commit (`@`); never split existing (parent) commits.
- Only describe changes visible in `jj diff`. Never mention changes that are NOT in the diff (e.g., if something was removed in a prior commit and doesn't appear in the current diff, don't say "remove ...").
- Show the user what action you have taken afterward.
- Confirm with the user if you're unsure.

## Process

1. **Analyze changes**:
   - Run `jj diff -s` to see changed files
   - If the user specified specific files or paths, focus on those
   - Run `jj log -r ::@ -n 20 --no-graph -T 'description ++ "\n---\n"'` for message style
   - Use `jj diff` for full diff view if needed
   - Analyze: Are changes logically related or distinct? Different subsystems/features? Mixed concerns (refactor + feature, fix + cleanup)?

2. **Determine if split is needed**:
   - If changes are logically distinct, split them
   - Use `jj op` to note the current operation ID (for rollback if split goes wrong)
   - Execute `jj split -r <id> -m "<commit-message>" -- <file>` for each split
   - Do not use interactive `jj split`

3. **Execute the commit**:
   - For a single commit: `jj commit -m "<message>"`
   - Use `jj describe <id> -m "<message>"` only when updating a description without moving on
   - Keep commit messages short and concise:
     - Subject line: 50-72 characters max (Git standard)
     - Use imperative mood ("add feature" not "added feature")
     - Body: explain "what" and "why", not "how"
   - Try to include a short summary of the change in the commit description, including "why" if available.
   - After `jj commit`, the new working copy (`@`) is ready for new changes.
   - After committing, run `jj log -r @- -n 1` to confirm

4. **Report what you did**:
   - Show the resulting commit(s) with `jj log -r @- -n 1` (or more if split)
   - Summarize the action taken (committed, split into N commits, etc.)

## Output Format

For each commit:

1. **Commit message** following repo's existing style
2. If the commit has been split: the reasoning and scope of the commit
