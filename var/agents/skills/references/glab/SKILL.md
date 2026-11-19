---
name: glab
type: reference
description: Local guardrails and gotchas for GitLab CLI (glab). ALWAYS read BEFORE using glab commands.
---

## GitLab CLI Guardrails (`glab`)

Run `glab <subcommand> --help` (or `man glab-<subcommand>`) for exact command syntax, subcommands, and flags. Apply these local rules:

### Safety & Permissions

- **Read vs. write boundary**: Read operations run without prompting. Write operations (creating, updating, commenting, closing, or merging issues/MRs/releases, or triggering/retrying CI pipelines) require explicit user request and confirmation.
- **Never delete repos**: Never run `glab repo delete`.
- **Always pass `-X GET` on read-only `glab api` calls**: Passing write methods (`-X`/`--method`) or data flags (`-f`/`--raw-field`, `-F`/`--field`, `--form`, `--input`) mutates state and requires confirmation.

### Non-Interactive & Flag Gotchas

- **Always pass `-R owner/repo` explicitly**: Accepts `OWNER/REPO`, `GROUP/SUBGROUP/REPO`, or full repo URL. Identify the target via `jj git remote list`.
- **Prevent interactive prompts on writes**: Pass explicit `-t`, `-d`, `-m`, and `-y` (`--yes`) flags so `glab` never opens `$EDITOR` or blocks on stdin.
- **Prefer `--output json` over `-F json` or `-O json`**: `glab issue list` binds `-O` to `--output` and `-F` to `--output-format`, whereas other subcommands bind `-F` to `--output`. Use `--output json | jaq .` consistently.
