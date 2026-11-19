---
name: github-cli
type: reference
description: Local guardrails and gotchas for GitHub CLI (gh). ALWAYS read BEFORE using gh commands.
---

## GitHub CLI Guardrails (`gh`)

Run `gh <subcommand> --help` (or `man gh-<subcommand>`) for exact command syntax, subcommands, and flags. Apply these local rules:

### Safety & Permissions

- **Read vs. write boundary**: Read operations run without prompting. Write operations (creating, editing, commenting, closing, or merging issues/PRs/releases, or rerunning/canceling Actions) require explicit user request and confirmation.
- **Never delete repos**: Never run `gh repo delete`.
- **Always pass `-X GET` on read-only `gh api` calls**: Passing `-f` or `-F` without `-X GET` silently switches `gh api` to `POST`. Only use `POST`/`PUT`/`PATCH`/`DELETE` or GraphQL `mutation` queries when explicitly requested.

### Non-Interactive & Targeting Rules

- **Always pass `-R owner/repo` explicitly**: Do not rely on implicit repository detection. Identify `owner/repo` via `jj git remote list`.
- **Prevent interactive prompts on writes**: Always pass explicit `-t "<title>"` and `-b "<body>"` flags (and `-B`/`-H` on `gh pr create`) so `gh` never opens `$EDITOR` or blocks on stdin.
- **Structured output**: Prefer `--json <fields>` with `--jq` or pipe to `jaq` over scraping human-formatted tables.
