---
name: code-check
description: Run combined project validation checks. Use when asked to check the project, run all relevant validation, or verify tests plus lint/static checks.
---

Run the relevant non-destructive project validation commands.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified files or paths, focus on those.

2. Detect validation commands in this order:
   a. Project instructions: `README.md`, `CONTRIBUTING.md`, `CLAUDE.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md`.
   b. Task runners: `Makefile`, `justfile`, `Taskfile.yml`.
   c. Wrapper scripts: `bin/`, `.my/bin/` (`check`, `test`, `lint`, `fmt`, `format`, etc.).
   d. Package manager scripts: `package.json`, `pyproject.toml`, `Cargo.toml`, `go.mod`, `Gemfile`, etc.
   e. Common defaults:
   - JavaScript/TypeScript: `npm test`, `npm run lint`, `npx tsc --noEmit`.
   - Python: `pytest`, `ruff check .`, `ruff format --check .`.
   - Go: `go test ./...`, `go vet ./...`, `golangci-lint run` when configured.
   - Rust: `cargo test`, `cargo clippy`, `cargo fmt --check`.
   - Ruby: `bundle exec rake test`, `bundle exec rspec`, `bundle exec rubocop`.
   - Nix: `nix flake check path:.` for flakes; use `path:` and read the `flake` skill first.

3. Run selected non-destructive commands:
   - Prefer project-provided aggregate checks over duplicating individual commands.
   - Prefer the most specific commands that cover the requested files or changes.
   - Use proper timeouts.
   - Do not use long-running watch modes.
   - Do not write formatting changes unless explicitly requested.

4. Handle failures:
   - Read the full error message before changing anything.
   - Identify the root cause.
   - If the user asked to fix issues, apply the smallest targeted fix and re-run the relevant command.
   - If the user only asked to run/check, report the failure and suggested fix without editing.

5. Stop condition:
   - If a fix fails twice, stop, provide root-cause analysis, and ask for guidance.

## Output

1. **Commands Used**
2. **Results**: pass/fail summary
3. **Failures** with root cause and file references
4. **Fixes Applied** if explicitly requested
5. **Remaining Issues** requiring manual action
6. **Verification** from re-running relevant commands
