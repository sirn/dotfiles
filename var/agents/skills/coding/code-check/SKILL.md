---
name: code-check
description: Detect and run project test, lint, and format commands; diagnose and fix failures when requested. Use when asked to run tests, lint, format, check, verify, or fix failing tests/lint/format/check commands.
---

Run project checks (tests, linting, formatting) and, when requested, diagnose and fix failures with minimal targeted changes.

## Process

### Step 1 - Identify Context

- For code changes: run `jj diff -s` to list changed files; use `jj diff -- path` to restrict to specific files/directories.
- Focus on user-specified files, commands, or failures. Read any provided failure output fully before making changes.

### Step 2 - Detect Command

Detect test, lint, and format commands in this order (single shared list, not duplicated per kind):

1. **Project instructions**: `README.md`, `CONTRIBUTING.md`, `CLAUDE.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md`.
2. **Task runners**: `Makefile`, `justfile`, `Taskfile.yml`.
3. **Wrapper scripts**: `bin/`, `.my/bin/` (`test`, `lint`, `check`, `fmt`, `format`, `*-test`, `*-lint`, `*-fmt`).
4. **Package manager scripts**: `package.json`, `pyproject.toml`, `Cargo.toml`, `go.mod`, `Gemfile`, etc.
5. **Common defaults by language**:
   - **JavaScript/TypeScript**:
     - Test: `npm test`
     - Lint: `npm run lint`, `npx tsc --noEmit`
     - Format: `npm run format`, `prettier --check .` (check) / `prettier --write .` (write)
   - **Python**:
     - Test: `pytest`
     - Lint: `ruff check .`, `mypy .` (when configured)
     - Format: `ruff format --check .` (check) / `ruff format .` (write)
   - **Go**:
     - Test: `go test ./...`
     - Lint: `golangci-lint run`, `go vet ./...`
     - Format: `gofmt` (check) / `gofmt -w` (write)
   - **Rust**:
     - Test: `cargo test`
     - Lint: `cargo clippy`
     - Format: `cargo fmt --check` (check) / `cargo fmt` (write)
   - **Ruby**:
     - Test: `bundle exec rake test`, `bundle exec rspec`
     - Lint: `bundle exec rubocop`
   - **Nix**:
     - Test/Lint/Check: `nix flake check path:.` for flakes (read the `nix` reference first)
     - Format: `nix run path:.#treefmt -- --ci` (check) / `nix run path:.#treefmt --` (write)

### Step 3 - Run Checks

- Default to check-only; do not edit unless explicitly asked to fix (or to apply formatting).
- Prefer the most specific command covering the requested files or changes.
- Use proper timeouts; avoid long-running watch modes.

### Step 4 - Modes

- **Run-only** (default): run, report pass/fail, and suggest fixes without editing.
- **Fix**: when asked to fix failures, proceed to Step 5. For formatting, applying the formatter is the fix.

### Step 5 - Diagnose and Fix (fix mode only)

Prefer the exact failing command from the user or project output. Before editing, diagnose the root cause.

Spawn the `researcher` subagent:

```
Research the root cause of this failure:
{error output}

Identify whether this is:
- a product-code bug
- a test bug
- an environment issue
- a stale expectation

Find relevant docs or known issues.
```

Then spawn the `worker` subagent:

```
Apply a minimal fix for the following in the specified file:
{root cause}
{file}

Do not weaken assertions, skip tests, or broaden ignores unless explicitly approved with sound rationale.
```

### Step 6 - Verify

Re-run the relevant command(s) to confirm the fix.

### Step 7 - Stop Condition

If a fix fails twice, stop, analyze the root cause, and ask for guidance.

### Step 8 - Report

Report the following to the user:

1. **Command Used**
2. **Mode**: run-only or fix
3. **Results**: pass/fail summary
4. **Failures** with root cause and file references
5. **Fixes Applied** (if fix mode)
6. **Remaining Issues** requiring manual action
7. **Verification** from re-running relevant commands

## Guardrails

- Run-only by default; do not edit unless explicitly asked to fix or to apply formatting.
- Do not weaken assertions, skip tests, or broaden ignores to make failures pass.
- Use the exact failing command from the user or project output when available.
- Avoid watch modes; use bounded timeouts.
- Prefer the most specific command covering the requested scope.
- For runtime/production errors and symptoms not surfaced by a check command, use `code-debug` instead.