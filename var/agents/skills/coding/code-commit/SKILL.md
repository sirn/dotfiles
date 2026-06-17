---
name: code-commit
description: Commit current changes using jj. Analyzes changes, suggests commit messages following repository conventions, splits when needed, and creates commits.
---

Commit current changes using Jujutsu (jj).

## Important

- Before running any `jj` command, read the `jujutsu` skill first

## Process

### Step 0 - Load Jujutsu Skill

- Read the `jujutsu` skill file before running any `jj` commands.
- Follow its Best Practices for local commit autonomy, revision references, logical grouping, and commit message style.

### Step 1 - Analyze Changes

- Run `jj diff -s` to see changed files
- Run `jj diff` for the full diff
- If the user specified specific files or paths, focus on those

### Step 2 - Analyze Existing Convention

- Run `jj log -r ::@ -n 20 --no-graph -T 'description ++ "\n---\n"'` to extract the commit message CONVENTION:
  - Identify the scope prefix pattern: analyze how existing commits structure their subject line — look for recurring prefixes (e.g., path-based like `<dir>/<name>:`, module names, component names, or conventional prefixes like `feat:`, `fix:`)
  - Identify whether conventional commit prefixes (feat:, fix:, chore:) are used — if they are absent, DO NOT add them
  - Note the mood and separator conventions from existing commits

### Step 3 - Execute Commit

- The commit message MUST be derived primarily from the diff itself
- The conversation context (what the user asked for) MUST not be included unless the commit itself is unobvious
- If changes are logically distinct (different subsystems, features, or concerns), mark them for split:
  - Use `jj op` to note the current operation ID (for rollback if split goes wrong)
  - Execute `jj split -r <id> -m "<commit-message>" -- <file>` for each split
  - Message MUST always be present with splitting a commit
  - Do NOT use interactive `jj split`
- For a single commit:
  - Run `jj commit -m "<message>"`
  - Use `jj describe <id> -m "<message>"` only when updating a description without creating a new empty commit on top
  - Follow the `jujutsu` skill's **Commit Messages** best practice for line length and style.
  - After `jj commit`, the new working copy (`@`) is ready for new changes.

### Step 4 - Verify Line Lengths

- A `check-commit-message.sh` is available within this skill directory.
  - Use `./check-commit-msg.sh [REV]` where `REV` can be a jujutsu revision, jujutsu alias, git revision, or git refs.
  - Use `./check-commit-msg.sh -h` to see all available options
- If any line exceeds the limit, fix it with `jj describe <rev> -m "<fixed-message>"` or `git commit --amend`.

### Step 5 - Report

- Show the resulting commit(s) with `jj log -r @- -n 1` (or more if split)
- For each commit, show its message and, if split, the reasoning and scope
