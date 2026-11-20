---
name: code-commit
description: Use when committing working-copy changes, writing commit messages, or separating unrelated edits into commits (prefers jj, supports git).
---

Commit working-copy changes directly, preferring Jujutsu (`jj`) with `git` fallback.

Most commits belong on a single line. Adding a body to routine commits buries the rare bodies that carry real rationale, so a body exists only when the diff cannot explain itself.

## Prerequisites

- Detect the VCS from the repository root: use `jj` when `jj root` succeeds; otherwise use `git`.
- When using `jj`, read the `jujutsu` skill first and follow its Autonomy & Safety and Non-Interactive Agent Rules.
- Commit directly without asking for message approval unless a guardrail below requires confirmation.

## Commit Message Contract

### Format

- **Default (single line)**: `<scope>: <imperative summary>` (<= 72 characters; omit `<scope>:` if the repository does not use scopes).
- **With body (exception only)**:

  ```text
  <subject line, <= 72 chars>

  <1 to 3 lines, <= 72 chars each>
  ```

### When a Body Is Allowed

Include a body only when at least one of these is true and is not obvious from the diff:

- A non-obvious constraint, upstream quirk, or bug root cause drove the change.
- A reasonable alternative was rejected and future readers need to know why.
- The change requires a breaking-change note, migration step, or issue reference per repository rules.

If none of these apply, stop after the subject line. If a body needs more than 3 lines, stop and justify the extra length to the user before committing.

### Examples

- **Routine multi-file feature (single line, no body)**: `api: add per-IP rate limiting to public routes`
- **Non-obvious constraint (subject + short body)**:

  ```text
  cache: reduce token TTL from 60s to 5s

  Upstream API v2.3 invalidates tokens silently after 10s, causing 401
  bursts under the previous TTL.
  ```

- **Bad (bloated changelog restating the diff)**:

  ```text
  profiles/terra: enable wireguard support and update packages

  - Bumped lofi to v0.2.2
  - Added wireguard configuration in nixos/system.nix
  - Updated terra profile to include wireguard
  ```

- **Good (split by logical change, single line each)**:
  - `nixos/wireguard: add peer-to-peer tunnel config`
  - `profiles/terra: enable wireguard`
  - `pkgs/lofi: update to 0.2.2`

## Process

### Step 1 - Inspect Working Copy

- `jj`: run `jj status` and `jj diff -s`, then `jj diff` for details.
- `git`: run `git status --short` and `git diff --stat` (plus `git diff --cached --stat`), then `git diff`.
- If there are no changes to commit, stop and report that the working copy is clean.
- Check for untracked, temporary, or sensitive files (`.env`, secrets, debug dumps). Because `jj` snapshots all non-ignored files automatically, exclude stray files by passing explicit paths to `jj commit`.
- In `jj`, if `@` already has a description (`jj log -r @ --no-graph -T description`), preserve any user-written intent or ticket references.

### Step 2 - Extract Repository Conventions

Check both sources in priority order:

1. **Documented repository rules (highest priority)**: Check root instruction files (`AGENTS.md`, `CLAUDE.md`, `CONTRIBUTING.md`, `GEMINI.md`) for explicit scope prefixes, formatting, or commit guidelines.
2. **Recent history**:
   - `jj`: `jj log -r '::@ ~ @ ~ root()' -n 20 --no-graph -T 'description ++ "\n---\n"'`
   - `git`: `git log -n 20 --format='%B---'`

Match the repository's scope prefix pattern (path-based, subsystem, brace scopes, or none), capitalization, mood, and separator style. Include type prefixes (such as `feat:` or `fix:`) only when documented or present in recent history.

### Step 3 - Group and Commit

- Derive messages from the diff first; use conversation context only for non-obvious rationale that qualifies for a body.
- Commit each logically distinct change separately:
  - `jj` (all changes): `jj commit -m "<msg>"`
  - `jj` (subset or split): `jj commit -m "<msg>" -- <paths>...` repeated once per logical group. Do not run `jj split` on uncommitted `@` changes; `jj split` rewrites an existing revision and requires user confirmation per `jujutsu`.
  - `git`: commit already-staged changes, or stage each logical group with `git add -- <paths>...` and run `git commit -m "<msg>"`.
- Never invoke commands that open an interactive editor.

### Step 4 - Validate Commit Messages

Run `check-commit-msg.sh` from the repository root through `bash` (never `cd` into the skill directory):

- `jj`: `bash <skill-dir>/check-commit-msg.sh @-` (or pass all created revisions, e.g. `@-- @-` or `'@--::@-'`).
- `git`: `bash <skill-dir>/check-commit-msg.sh HEAD` (when creating multiple `git` commits, validate `HEAD` after each commit so `git commit --amend` remains safe).

The script verifies non-empty messages, <= 72 characters per line, and a blank second line when a body is present. If validation fails, fix and re-run until it exits `0`:

- `jj`: `jj describe <rev> -m "<fixed-message>"`
- `git`: `git commit --amend -m "<fixed-message>"` (for the latest local commit only)

### Step 5 - Report

- Display created commit(s) with `jj log -r '<revs>'` (e.g. `jj log -r @- -n 1`) or `git log -n <count> --oneline`.
- Report each commit message, the grouping rationale if split, and any stray files intentionally left uncommitted.

## Guardrails

- Never push, create unsolicited bookmarks/branches, or rewrite immutable or shared history.
- Never commit secrets, credentials, `.env` files, or debug/temporary artifacts.
- Never run `jj split` without explicit user confirmation; use path-scoped `jj commit` for separating working-copy changes.
- Never write a commit body to summarize the diff or list files; stop and ask before exceeding 3 body lines.
- Never skip `check-commit-msg.sh` after committing.
