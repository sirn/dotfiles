---
name: code-generate
description: Generate tests, documentation, ADRs, or CI/CD configuration. Use when asked to add tests, create documentation, write ADRs, or generate pipeline config.
---

Generate project artifacts that match existing conventions.

## Modes

- **Tests**: Generate tests for untested functions and edge cases, then run the relevant test command directly.
- **Docs**: Generate or update docstrings, module docs, README sections, or other documentation.
- **ADR**: Create an Architecture Decision Record.
- **CI**: Generate CI/CD pipeline configuration.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified files, modules, or requirements, focus on those.
   - Determine mode from the request.

2. Inspect existing conventions:
   - Read relevant code and neighboring files.
   - Check existing tests, docs, ADRs, or CI files for naming, layout, style, helpers, fixtures, and task ordering.
   - Check project instructions: `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md`.

3. Execute by mode:

   **Tests**:
   - Detect test framework and command from instructions, task runners, wrapper scripts, package manager scripts, and common defaults.
   - Identify behavior to test through public interfaces when possible.
   - Cover happy paths, edge cases, and error paths that are observable.
   - Keep tests simple and explicit; avoid unnecessary helpers or abstractions.
   - Add tests matching existing naming and fixture conventions.
   - Run the most specific relevant test command directly.
   - Fix generated test failures if the fix is clear; stop after two failed attempts.

   **Docs**:
   - Determine documentation type: inline docs, module/file docs, README content, or generated reference material.
   - Match existing documentation style.
   - Write comments that explain why and intent, not obvious mechanics.
   - Avoid changelog-style comments.

   **ADR**:
   - Ask for missing Context, Decision, or Consequences if not provided.
   - Use the project's ADR directory and naming convention if one exists; otherwise propose `doc/adr/` or `docs/adr/`.
   - Include Title, Status, Context, Decision, Consequences, and Alternatives when useful.

   **CI**:
   - Identify CI provider: GitHub Actions, GitLab CI, Jenkins, etc.
   - Locate existing config files.
   - Research current official syntax/version requirements with WebSearch/WebFetch when needed.
   - Generate a minimal pipeline for the requested goal, typically lint/check → test → build → deploy only when required.
   - Check for insecure permissions, secret leaks, script injection, unsafe pull-request handling, and poor cache keys.

4. Apply changes only within the requested scope.

5. Verify:
   - Tests mode: run generated tests.
   - Docs/ADR mode: check formatting or markdown lint if configured.
   - CI mode: run YAML/schema validation if available; otherwise explain manual verification steps.

## Output

1. **Mode**
2. **Conventions Detected**
3. **Generated Content / Files Changed**
4. **Verification Results**
5. **Remaining Follow-up**
