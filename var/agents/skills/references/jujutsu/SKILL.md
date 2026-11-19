---
name: jujutsu
type: reference
description: Local Jujutsu (jj) safety guardrails, non-interactive command rules, and gotchas. ALWAYS read before performing ANY jj operation (commit, squash, rebase, describe, split, etc.).
---

## Jujutsu Guardrails & Workflow

Run `jj <subcommand> --help` (or `man jj-<subcommand>`) for exact command syntax, flags, and revset functions (`jj help -k revsets`, `jj help -k templates`). Follow these local rules for every `jj` operation:

### Autonomy & Safety

- **Local commits allowed**: `jj describe`, `jj commit`, and `jj new` are allowed when part of the current requested task and staying local. Routine local amends (`jj squash --into @- -u`) do not need confirmation.
- **Require explicit user confirmation**: NEVER run `jj git push`, or destructive/history-rewriting operations (`jj edit`, `jj squash` across bookmarks/shared history, `jj split`, `jj rebase`, `jj abandon`, `jj undo`, `jj op restore`, bookmark moves/deletes, tag moves/deletes) without explicit user confirmation.
- **No unsolicited bookmarks**: NEVER run `jj bookmark create` unless the user explicitly asks, and create only the bookmark(s) requested.
- **Never pass `--ignore-immutable`**: Commits in `::immutable_heads()` (including `trunk()` ancestors) are immutable.
- **Empty `@` is normal**: After `jj commit` or `jj new`, a new empty commit becomes `@`. Do not remove or squash it away.

### Non-Interactive Agent Rules

- **Prefer `jj commit -m "msg"`**: Finalize `@` and advance in one step instead of `jj describe` + `jj new`.
- **Commit messages**: Subject and body lines <= 72 characters; imperative mood ("add feature", not "added feature"); explain "what" and "why", not "how".
- **Non-interactive `jj squash`**: When both source and destination have non-empty descriptions and the source is abandoned, `jj squash` opens `$EDITOR`. ALWAYS pass `-u` (`--use-destination-message`) or `-m "<message>"`.
- **Squash direction**: Squash newer into older (`jj squash --from <child> --into <parent> -u`) to avoid rewriting ancestors and triggering rebase conflicts.
- **Non-interactive `jj split`**: Always pass `-m "<msg>" -- <paths>...`; never run interactive `jj split`.
- **Revision references**: Use `@` / `@-` for simple one-off commands, and explicit change IDs for multi-step or destructive operations.

### Common Gotchas & Idioms

- **Hidden immutable history**: Default `jj log` hides immutable ancestors; pass `-r ::` or `all()` to see full history.
- **Repository/remote detection**: Run `jj git remote list` (parse `owner/repo` for `gh`/`glab`).
- **Executable bits**: Use `jj file chmod x <path>` / `jj file chmod n <path>` to change permissions cleanly across platforms.
- **Recovery distinctions**:
  - `jj restore` copies file contents from another revision without rewriting history (`jj restore --changes-in <id>` reverts a commit's diff into `@`).
  - `jj revert -r <id> -o <dest>` creates a new commit with the inverse diff.
  - `jj op log` / `jj op show <op-id>` inspects past operations (only run `jj undo` / `jj op restore` with confirmation).
- **Divergent changes (`??`)**: Two visible commits share a change ID. Inspect and resolve by explicit commit ID:

  ```bash
  jj log -r 'divergent()'
  jj interdiff --from <change-id>/0 --to <change-id>/1
  # Keep one and abandon the obsolete commit (after confirmation):
  jj abandon <obsolete-commit-id>
  # Or keep both as distinct changes:
  jj metaedit -r <commit-id> --update-change-id
  ```
