---
name: code-commit
description: Commit current changes using jj. Analyzes changes, suggests commit messages following repository conventions, splits when needed, and creates commits.
---

Commit current changes using Jujutsu (jj).

## Important

- Read the `jujutsu` skill before running any `jj` command.

## Commit Message Style

Commit messages must provide a high-level overview of the change, NOT a detailed breakdown.

- **Focus on intent**: Explain the 'why' and 'what' of the change at a high level, specifying the affected subsystem or concern. Reinforce that the commit message should capture the _Why_ of the change (the reason/motivation for making it), as distinct from the _How_ (described by code) and _What_ (described by test code).
- **Do not enumerate changes**: Do not list the files changed, the number of files, or what was changed file-by-file. The diff already shows these details.
- **Keep it concise**: Ensure the reader can understand the purpose of the change at a glance without re-reading the diff.
- Follow the `jujutsu` reference for line length, mood, and style conventions.

### Examples

- **Bad**: `update flake.nix, profiles/terra.nix, and nixos/system.nix to bump package versions and add wireguard module`
- **Good**: `profiles/terra: enable wireguard support and update packages`

- **Bad**: `fix typo in home-manager/config/git.nix line 42 and rename git.nix to git-config.nix`
- **Good**: `hm/git: correct configuration typos and rename module`

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

- Derive the commit message primarily from the diff, not the conversation context, unless context explains an unobvious change. Follow the requirements in the **Commit Message Style** section.
- If changes are logically distinct, split them (one commit message per split, do NOT use interactive `jj split`).
- Refer to the `jujutsu` reference for the exact `jj commit` / `jj split` syntax and the commit+advance workflow.
- Note that `jj commit` leaves the new working-copy `@` ready for new changes.

### Step 4 - Verify Line Lengths

- Use the `check-commit-msg.sh` script located in this skill directory:
  - Reference it by its absolute path. Do NOT `cd` into the skill directory, as `jj` operations must run in the repository's working directory.
  - Run `/path/to/check-commit-msg.sh [REV]` where `REV` can be a jujutsu revision, jujutsu alias, git revision, or git refs.
  - Run `/path/to/check-commit-msg.sh -h` to see all available options.
- If any line exceeds the limit (per the `jujutsu` reference), fix with `jj describe <rev> -m "<fixed-message>"`.

### Step 5 - Report

- Display the resulting commit(s) using `jj log -r @- -n 1` (or more if split).
- For each commit, report its message and, if split, the reasoning and scope.
