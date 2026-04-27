---
name: code-test
description: Detect and run project tests, linting, formatting, and checks. Use when asked to test, lint, check style, format, or verify validation commands.
---

Run project validation commands and diagnose failures.

## Modes

- **Test** (default): Detect and run tests.
- **Lint**: Detect and run linting/static checks, with auto-fix when explicitly requested.
- **Format**: Detect and run project formatting.
- **Check**: Run the relevant test and lint/check commands.
- **Fix**: Diagnose and fix test/lint failures when the user explicitly asks for fixes.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified files or paths, focus on those.
   - Determine mode from the request; default to **Test**.

2. Detect commands in this order:
   a. Project instructions: `README.md`, `CONTRIBUTING.md`, `CLAUDE.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md`.
   b. Task runners: `Makefile`, `justfile`, `Taskfile.yml`.
   c. Wrapper scripts: `bin/`, `.my/bin/` (`test`, `lint`, `fmt`, `format`, `check`, `*-test`, `*-lint`, etc.).
   d. Package manager scripts: `package.json`, `pyproject.toml`, `Cargo.toml`, `go.mod`, `Gemfile`, etc.
   e. Common defaults:
   - JavaScript/TypeScript: `npm test`, `npm run lint`, `npm run format`, `npx tsc --noEmit`.
   - Python: `pytest`, `ruff check .`, `ruff format --check .`.
   - Go: `go test ./...`, `golangci-lint run`, `gofmt -w` only when formatting was requested.
   - Rust: `cargo test`, `cargo clippy`, `cargo fmt --check`.
   - Ruby: `bundle exec rake test`, `bundle exec rspec`, `bundle exec rubocop`.
   - Nix: `nix flake check path:.` for flakes; use `path:.`.

3. Run the selected command(s):
   - Prefer the most specific command that covers the requested files or changes.
   - Use proper timeouts.
   - Do not use long-running watch modes.
   - In read-only/planning contexts, report results only.

4. Handle failures:
   - Read the full error message before changing anything.
   - Identify the root cause.
   - If the user asked to fix issues, apply the smallest targeted fix and re-run the relevant command.
   - If the user only asked to run/check, report the failure and suggested fix without editing.

5. Stop condition:
   - If a fix fails twice, stop, provide root-cause analysis, and ask for guidance.

## Output

1. **Mode** and command(s) used
2. **Results**: pass/fail summary
3. **Failures** with root cause and file references
4. **Fixes Applied** if explicitly requested
5. **Remaining Issues** requiring manual action
6. **Verification** from re-running relevant commands
