---
name: code-format
description: Detect and run project formatting commands. Use when asked to format code or check formatting.
---

Run project formatting commands.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified files or paths, focus on those.
   - Determine whether the user asked to check formatting or write formatting changes.

2. Detect formatting commands in this order:
   a. Project instructions: `README.md`, `CONTRIBUTING.md`, `CLAUDE.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md`.
   b. Task runners: `Makefile`, `justfile`, `Taskfile.yml`.
   c. Wrapper scripts: `bin/`, `.my/bin/` (`fmt`, `format`, `*-fmt`, etc.).
   d. Package manager scripts: `package.json`, `pyproject.toml`, `Cargo.toml`, `go.mod`, `Gemfile`, etc.
   e. Common defaults:
   - JavaScript/TypeScript: `npm run format` or `prettier --check .` / `prettier --write .` when configured.
   - Python: `ruff format --check .` or `ruff format .`.
   - Go: `gofmt` in check mode when possible, `gofmt -w` only when formatting was requested.
   - Rust: `cargo fmt --check` or `cargo fmt`.
   - Ruby: `bundle exec rubocop --format simple` for checks, autofix only when requested.
   - Nix: project formatter such as `nix run path:.#treefmt -- --ci` for checks or `nix run path:.#treefmt --` for formatting; read the `flake` skill first.

3. Run the selected command:
   - Use check-only formatting unless the user explicitly asked to format/write changes.
   - Prefer the most specific command that covers the requested files or changes.
   - Use proper timeouts.
   - Do not use long-running watch modes.

4. Handle failures:
   - Read the full error message before changing anything.
   - If the user only asked to check formatting, report the failure without editing.
   - If the user asked to format, apply the formatter and report changed files.

## Output

1. **Command Used**
2. **Mode**: check-only or write formatting
3. **Results**: pass/fail summary
4. **Files Formatted** if formatting was requested
5. **Remaining Issues** requiring manual action
6. **Verification** from re-running relevant commands when appropriate
