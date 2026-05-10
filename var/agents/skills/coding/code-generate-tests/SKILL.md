---
name: code-generate-tests
description: Generate or update tests for observable public behavior. Use when asked to add tests, cover edge cases, or improve test coverage.
---

Generate tests that match existing project conventions.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified files, modules, behavior, or requirements, focus on those.
   - Identify the public behavior and interfaces to test.

2. Inspect existing conventions:
   - Read relevant code, neighboring tests, fixtures, and helpers.
   - Detect the test framework and command from instructions, task runners, wrapper scripts, package manager scripts, and common defaults.
   - Check project instructions: `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md`.

3. Generate tests:
   - Prefer public interfaces and observable behavior over private implementation details.
   - Cover happy paths, edge cases, and error paths that are observable.
   - Match existing naming, fixture, assertion, and file-layout conventions.
   - Keep tests simple and explicit; avoid unnecessary helpers or abstractions.

4. Apply changes only within the requested scope.

5. Verify:
   - Run the most specific relevant test command directly.
   - Fix generated test failures if the fix is clear; stop after two failed attempts.

## Output

1. **Behavior Covered**
2. **Conventions Detected**
3. **Tests Added / Files Changed**
4. **Verification Results**
5. **Remaining Follow-up**
