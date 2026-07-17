---
name: code-commit
description: Commit current changes using jj. Analyzes changes, suggests commit messages following repository conventions, splits when needed, and creates commits.
---

Commit current changes using Jujutsu (jj).

## Important

- Read the `jujutsu` skill before running any `jj` command.

## Commit Message Style

Commit messages provide a high-level overview of the change, NOT a detailed breakdown. They capture the _Why_ (reason/motivation), distinct from the _How_ (code) and _What_ (test code).

- **Focus on intent**: the high-level why and the affected subsystem or concern; do not enumerate file-level what.
- **Do not enumerate changes**: no file lists, file counts, or file-by-file detail; the diff shows these.
- **Keep it concise**: reader understands the purpose at a glance without re-reading the diff.
- Follow the `jujutsu` reference for line length, mood, and style conventions.

### Examples

- **Bad**: `update flake.nix, profiles/terra.nix, and nixos/system.nix to bump package versions and add wireguard module`
- **Good**: `profiles/terra: enable wireguard support and update packages`
- **Bad**: `fix typo in home-manager/config/git.nix line 42 and rename git.nix to git-config.nix`
- **Good**: `hm/git: correct configuration typos and rename module`

## Process

### Step 0 - Load Jujutsu Skill

- Review the `jujutsu` skill and follow its Best Practices for local commit autonomy, revision references, logical grouping, and message style.

### Step 1 - Analyze Changes

- `jj diff -s` to inspect changed files; `jj diff` for the full diff.
- Focus on any user-specified files or paths.

### Step 2 - Analyze Existing Convention

- `jj log -r ::@ -n 20 --no-graph -T 'description ++ "\n---\n"'` to extract conventions:
  - Identify the scope prefix pattern (path-based like `<dir>/<name>:`, module, or component names).
  - Determine if conventional prefixes (`feat:`, `fix:`, `chore:`) are used; omit them if absent from history.
  - Note mood and separator conventions.

### Step 3 - Execute Commit

- Derive the message primarily from the diff, not conversation context, unless context explains an unobvious change. Follow **Commit Message Style**.
- If changes are logically distinct, split them (one message per split; do NOT use interactive `jj split`).
- Refer to the `jujutsu` reference for exact `jj commit` / `jj split` syntax and the commit+advance workflow.
- `jj commit` leaves the new working-copy `@` ready for new changes.

### Step 4 - Verify Line Lengths

- Run the `check-commit-msg.sh` script in this skill directory:
  - Reference it by absolute path; do NOT `cd` into the skill directory (`jj` must run in the repo working directory).
  - `/path/to/check-commit-msg.sh [REV]` where `REV` is a jujutsu revision/alias or git revision/ref.
  - `/path/to/check-commit-msg.sh -h` for all options.
- If any line exceeds the limit (per `jujutsu`), fix with `jj describe <rev> -m "<fixed-message>"`.

### Step 5 - Report

- Display resulting commit(s) with `jj log -r @- -n 1` (or more if split).
- For each commit, report its message and, if split, the reasoning and scope.
