---
name: code-generate-tests
description: Generate or update tests for observable public behavior. Use when asked to add tests, cover edge cases, or improve test coverage.
---

Generate tests that match existing project conventions.

## Process

### Step 1 - Identify Context

- If code changes are involved, run `jj diff -s` first to see changed files, then use `jj diff -- path` to restrict to specific files/directories.
- Focus on any files, modules, behavior, or requirements specified by the user.
- Identify public behavior and interfaces to test.

### Step 2 - Research and Scout

Spawn `scout` subagent:

```
Identify critical public behavior, neighboring tests, fixtures, naming conventions, edge cases, and error paths in {files}.
```

Spawn `researcher` subagent:

```
Research idiomatic testing practices for the detected language/framework.
```

Spawn `reviewer` subagent:

```
Review proposed tests with a simplicity and behavior-coverage lens; avoid unnecessary helpers or private-implementation assertions.
```

### Step 3 - Inspect Conventions

- Read relevant code, neighboring tests, fixtures, and helpers.
- Detect the test framework and command from task runners, wrapper scripts, package manager scripts, or defaults.
- Check project instructions in `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, and `CODEX.md`.

### Step 4 - Generate Tests

- Prioritize testing public interfaces and observable behavior (covering happy paths, edge cases, and error paths) over private implementation details.
- Match existing naming, fixture, assertion, and file-layout conventions.
- Keep tests simple and explicit; avoid unnecessary helpers or abstractions.

### Step 5 - Verify

- Run the most specific test command directly.
- Fix failures if the cause is clear, but stop after two failed attempts.

### Step 6 - Report

Report to the user:

1. **Behavior Covered**
2. **Conventions Detected**
3. **Tests Added / Files Changed**
4. **Verification Results**
5. **Remaining Follow-up**
