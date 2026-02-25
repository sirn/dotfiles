---
name: code-lint
description: Detect project config, run linting, and fix issues. Use when user asks to lint or check style.
---

Run project linting by detecting the environment and fixing issues.

## Process
1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories
   - If the user specified specific files or paths, focus on those

2. Detect the preferred way to run linting:
   a. Check existing instructions: `README.md`, `CONTRIBUTING.md`, `CLAUDE.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md`
   b. Check task runners: `Makefile`, `justfile`, `Taskfile.yml` for lint target
   c. Check for scripts in `bin/`, `.my/bin/` (lint, check, *-lint, etc.)
   d. Check package manager scripts (`package.json` lint/check scripts, etc.)
   e. Fall back to standard: `npm run lint`, `ruff check .`, `golangci-lint run`, `cargo clippy`

3. Run the detected linting command with auto-fix where possible
4. Handle remaining issues manually

## Remaining Issues
For issues that require manual intervention:
1. Categorize by severity (critical > high > medium > low)
2. Provide specific fixes for each issue
3. Explain impact of each fix
4. Re-run lint to verify all issues are resolved

## Stop Condition
- If a fix fails twice, stop and ask for guidance.

## Output
1. **Lint command used**
2. **Auto-fixes applied**
3. **Remaining issues**
