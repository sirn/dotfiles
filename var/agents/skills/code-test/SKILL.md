---
name: code-test
description: Detect project config, run tests, and fix failures. Use when user asks to run tests or mentions testing.
---

Run project tests by detecting the environment and fixing failures.

## Process
1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories
   - If the user specified specific files or paths, focus on those

2. Detect the preferred way to run tests:
   a. Check existing instructions: `README.md`, `CONTRIBUTING.md`, `CLAUDE.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md`
   b. Check task runners: `Makefile`, `justfile`, `Taskfile.yml` for test target
   c. Check for scripts in `bin/`, `.my/bin/` (test, *-test, etc.)
   d. Check package manager scripts (`package.json` scripts, etc.)
   e. Fall back to standard: `npm test`, `pytest`, `go test ./...`, `cargo test`

3. Run the detected test command
4. Analyze and fix any failures

## Failure Handling
For any test failures:
1. Identify the root cause
2. Provide specific fixes
3. Explain why the fix works
4. Re-run tests to verify

## Stop Condition
- If a fix fails twice, stop and ask for guidance.

## Output
1. **Test command used**
2. **Failures and fixes** (if any)
3. **Verification**
