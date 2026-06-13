---
name: code-commit
description: Commit current changes using jj. Analyzes changes, suggests commit messages following repository conventions, splits when needed, and creates commits.
---

Commit current changes using Jujutsu (jj).

## Important

**IMPORTANT**: Before running any `jj` command, read the `jujutsu` skill first for current command syntax,
revision references, local commit autonomy rules, and logical grouping best practices.

**IMPORTANT**: The commit message MUST be derived primarily from the diff itself (`jj diff`).
The current conversation context (what the user asked for) is a secondary source only;
it MUST NOT override or replace what the diff actually shows. If the diff contains
changes not mentioned in the conversation, include them. If the conversation mentions
changes NOT in the diff, DO NOT include them.

## Process

### Step 0 - Load Jujutsu Skill

- Read the `jujutsu` skill file before running any `jj` commands.
- Follow its Best Practices for local commit autonomy, revision references, logical grouping, and commit message style.

### Step 1 - Analyze Changes

- Run `jj diff -s` to see changed files
- Run `jj diff` for the full diff
- If the user specified specific files or paths, focus on those
- Run `jj log -r ::@ -n 20 --no-graph -T 'description ++ "\n---\n"'` to extract the commit message CONVENTION:
  - Identify the scope prefix pattern: analyze how existing commits structure their subject line — look for recurring prefixes (e.g., path-based like `<dir>/<name>:`, module names, component names, or conventional prefixes like `feat:`, `fix:`)
  - Identify whether conventional commit prefixes (feat:, fix:, chore:) are used — if they are absent, DO NOT add them
  - Note the mood and separator conventions from existing commits

### Step 2 - Determine if Split is Needed

- If changes are logically distinct (different subsystems, features, or concerns), split them
- Use `jj op` to note the current operation ID (for rollback if split goes wrong)
- Execute `jj split -r <id> -m "<commit-message>" -- <file>` for each split
- Do not use interactive `jj split`

### Step 3 - Execute Commit

- For a single commit: `jj commit -m "<message>"`
- Use `jj describe <id> -m "<message>"` only when updating a description without moving on
- Follow the `jujutsu` skill's **Commit Messages** best practice for line length and style.
- After `jj commit`, the new working copy (`@`) is ready for new changes.

### Step 4 - Verify Line Lengths

- After committing, verify that every line in the commit message is <= 72 characters, e.g. `./check-commit-msg.sh [REV]` where `REV` can be a jujutsu revision, jujutsu alias, git revision, or git refs. Use `-h` to see all options.
- If any line exceeds the limit, fix it with `jj describe <rev> -m "<fixed-message>"` or `git commit --amend`.

### Step 5 - Report

- Show the resulting commit(s) with `jj log -r @- -n 1` (or more if split)
- For each commit, show its message and, if split, the reasoning and scope
