---
name: code-test
description: Detect and run project tests. Use when asked to run tests, verify test results, or execute the relevant test command.
---

Run project test commands and diagnose failures.

## Process

### Step 1 - Identify Context

- If code changes are involved: run `jj diff -s` to see changed files, then use `jj diff -- path` to focus on specific files/directories.
- Focus on user-specified files or paths when provided.

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

- Prefer the most specific command covering the requested files or changes.
- Use appropriate timeouts and avoid long-running watch modes.
- Report results only in read-only/planning contexts.

### Step 4 - Handle Failures

- Analyze the full error message to identify the root cause before editing.
- If asked to fix, apply the smallest targeted change and re-run the command.
- If asked only to run/check, report the failure and suggest a fix without editing.

### Step 5 - Stop Condition

- If a fix fails twice, stop to provide root-cause analysis and ask for guidance.

### Step 6 - Report

Report to the user:

1. **Command Used**
2. **Results**: pass/fail summary
3. **Failures** with root cause and file references
4. **Fixes Applied** if explicitly requested
5. **Remaining Issues** requiring manual action
6. **Verification** from re-running relevant commands
