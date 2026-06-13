---
name: code-lint
description: Detect and run linting or static-analysis commands. Use when asked to lint, run static checks, check style, or run non-format code quality checks.
---

Run project linting and static-analysis commands.

## Process
### Step 1 - Identify Context

- If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
- If the user specified files or paths, focus on those.

### Step 2 - Detect Lint Command

Detect lint/static-check commands in this order:
1. Project instructions: `README.md`, `CONTRIBUTING.md`, `CLAUDE.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md`.
2. Task runners: `Makefile`, `justfile`, `Taskfile.yml`.
3. Wrapper scripts: `bin/`, `.my/bin/` (`lint`, `check`, `*-lint`, etc.).
4. Package manager scripts: `package.json`, `pyproject.toml`, `Cargo.toml`, `go.mod`, `Gemfile`, etc.
5. Common defaults:
   - JavaScript/TypeScript: `npm run lint`, `npx tsc --noEmit`.
   - Python: `ruff check .`, `mypy .` when configured.
   - Go: `golangci-lint run`, `go vet ./...`.
   - Rust: `cargo clippy`.
   - Ruby: `bundle exec rubocop`.
   - Nix: `nix flake check path:.` for flakes; use `path:` and read the `flake` skill first.

### Step 3 - Run Linter

- Prefer the most specific command that covers the requested files or changes.
- Use proper timeouts.
- Do not use long-running watch modes.
- Run autofix only when explicitly requested.

### Step 4 - Handle Failures

- Read the full error message before changing anything.
- Identify the root cause.
- If the user asked to fix issues, apply the smallest targeted fix and re-run the relevant command.
- If the user only asked to run/check, report the failure and suggested fix without editing.

### Step 5 - Stop Condition


- If a fix fails twice, stop, provide root-cause analysis, and ask for guidance.
### Step 6 - Report

Report the following to the user:

1. **Command Used**
2. **Results**: pass/fail summary
3. **Failures** with root cause and file references
4. **Fixes Applied** if explicitly requested
5. **Remaining Issues** requiring manual action
6. **Verification** from re-running relevant commands
