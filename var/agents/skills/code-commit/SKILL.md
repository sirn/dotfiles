---
name: code-commit
description: Commit current changes using jj. Analyzes changes, suggests commit messages following repository conventions, proposes splits if needed, and creates commits.
---

Commit current changes using Jujutsu (jj).

## Important
**IMPORTANT**: Always use `jj` (Jujutsu) commands. Only fall back to `git` if jj is not available.
Refer to the `jj-reference` skill for command syntax and **Best Practices** (explicit change IDs, logical grouping, etc.).

## Process

1. **Analyze changes**:
   - Run `jj diff -s` to see changed files
   - If the user specified specific files or paths, focus on those
   - Run `jj log -r ::@ -n 20 --no-graph -T 'description ++ "\n---\n"'` for message style
   - Use `jj diff` for full diff view if needed
   - Analyze: Are changes logically related or distinct? Different subsystems/features? Mixed concerns (refactor + feature, fix + cleanup)?

2. **Determine if split is needed**:
   - If changes are logically distinct, propose splitting
   - Present the plan (messages, which files in each commit) to the user for approval before executing

3. **Execute the commit**:
   - For a single commit: `jj describe <id> -m "<message>"`
   - For splits: use `jj split -r <id> -m "<commit-message>" -- <file>` for each commit; do not use interactive `jj split`
   - Keep commit messages concise:
     - Subject line: 50-72 characters max (Git standard)
     - Use imperative mood ("add feature" not "added feature")
     - Body: explain "what" and "why", not "how"
   - Try to include a short summary of the change in the commit description, including "why" if available.
   - After the last commit is described/split, create a new empty commit with `jj new` so the working copy (`@`) is ready for new changes.
   - After committing, run `jj log -r @` to confirm

## Output Format

When analyzing, provide:
1. **Suggested commit message** following repo's existing style
2. **Should split?** Yes/No with reasoning
3. If split recommended: how to split (files/hunks per commit), message for each, `jj` commands
