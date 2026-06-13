---
name: code-test
description: Detect and run project tests. Use when asked to run tests, verify test results, or execute the relevant test command.
---

Run project test commands and diagnose failures.

## Process

### Step 1 - Identify Context

- If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
- If the user specified files or paths, focus on those.

### Step 2 - Detect Test Command

Detect test commands in this order:

1. Project instructions: `README.md`, `CONTRIBUTING.md`, `CLAUDE.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md`.
2. Task runners: `Makefile`, `justfile`, `Taskfile.yml`.
3. Wrapper scripts: `bin/`, `.my/bin/` (`test`, `*-test`, etc.).
4. Package manager scripts: `package.json`, `pyproject.toml`, `Cargo.toml`, `go.mod`, `Gemfile`, etc.
5. Common defaults:
   - JavaScript/TypeScript: `npm test`.
   - Python: `pytest`.
   - Go: `go test ./...`.
   - Rust: `cargo test`.
   - Ruby: `bundle exec rake test`, `bundle exec rspec`.

### Step 3 - Run Tests

- Prefer the most specific command that covers the requested files or changes.
- Use proper timeouts.
- Do not use long-running watch modes.
- In read-only/planning contexts, report results only.

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
