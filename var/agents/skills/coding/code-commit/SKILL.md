---
name: code-commit
description: Commit current changes using jj. Analyzes changes, suggests commit messages following repository conventions, splits when needed, and creates commits.
---

Commit current changes using Jujutsu (jj).

## Important

- Read the `jujutsu` skill before running any `jj` command.

## Process

### Step 0 - Load Jujutsu Skill

- Review the `jujutsu` skill file and follow its Best Practices for local commit autonomy, revision references, logical grouping, and commit message style.

### Step 1 - Analyze Changes

- Run `jj diff -s` to inspect changed files and `jj diff` for the full diff.
- Focus on any user-specified files or paths.

### Step 2 - Analyze Existing Convention

- Run `jj log -r ::@ -n 20 --no-graph -T 'description ++ "\n---\n"'` to extract commit message conventions:
  - Identify the scope prefix pattern (e.g., path-based like `<dir>/<name>:`, module, or component names).
  - Determine if conventional prefixes like `feat:`, `fix:`, or `chore:` are used; do not use them if they are absent in the history.
  - Note the mood and separator conventions from existing commits.

### Step 3 - Execute Commit

- Derive the commit message primarily from the diff, not the conversation context, unless the context explains an otherwise unobvious change.
- If changes are logically distinct (different subsystems, features, or concerns), split them:
  - Record the current operation ID with `jj op log -n 1` (revert with `jj undo` or `jj op restore <op-id>` if needed).
  - Run `jj split -r <id> -m "<commit-message>" -- <file>` for each split (do NOT use interactive `jj split`).
  - Always provide a commit message when splitting.
- For a single commit:
  - Run `jj commit -m "<message>"`.
  - Use `jj describe <id> -m "<message>"` only to update an existing description without creating a new empty commit.
  - Follow the `jujutsu` skill's **Commit Messages** best practice for line length and style.
  - Note that `jj commit` leaves the new working copy (`@`) ready for new changes.

### Step 4 - Verify Line Lengths

- Use the `check-commit-msg.sh` script located in this skill directory:
  - Reference it by its absolute path. Do NOT `cd` into the skill directory, as `jj` operations must run in the repository's working directory.
  - Run `/path/to/check-commit-msg.sh [REV]` where `REV` can be a jujutsu revision, jujutsu alias, git revision, or git refs.
  - Run `/path/to/check-commit-msg.sh -h` to see all available options.
- If any line exceeds the limit, fix it using `jj describe <rev> -m "<fixed-message>"`.

### Step 5 - Report

- Display the resulting commit(s) using `jj log -r @- -n 1` (or more if split).
- For each commit, report its message and, if split, the reasoning and scope.
