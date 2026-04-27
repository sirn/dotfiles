---
name: code-generate
description: Generate tests, documentation, ADRs, or CI/CD configuration using specialized subagents when available. Use when asked to add tests, create documentation, write ADRs, or generate pipeline config.
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

2. Spawn applicable agents in parallel:

   **Tests**:
   - `code-architect`: "Identify critical public behavior, edge cases, and error paths in {files}."
   - `code-researcher`: "Research idiomatic testing practices for the detected language/framework."
   - `simplicity-reviewer`: "Ensure proposed tests are minimal, boring, explicit, and avoid unnecessary helpers or abstractions."

   **Docs / ADR**:
   - `code-architect`: "Explain structure, data flow, design decisions, and architectural context for {files or requirements}."
   - `convention-reviewer`: "Identify existing documentation style, naming, and ADR conventions."

   **CI**:
   - `code-researcher`: "Research current official CI provider syntax, action versions, and cache patterns for {provider}."
   - `security-researcher`: "Review proposed CI design for secret leaks, script injection, unsafe permissions, and insecure pull-request handling."
   - `simplicity-reviewer`: "Keep the CI pipeline minimal; remove jobs, matrices, and cache layers not required by the current project."

3. Inspect existing conventions yourself:
   - Read relevant code and neighboring files.
   - Check existing tests, docs, ADRs, or CI files for naming, layout, style, helpers, fixtures, and task ordering.
   - Check project instructions: `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md`.

4. Execute by mode:

   **Tests**:
   - Detect test framework and command from instructions, task runners, wrapper scripts, package manager scripts, and common defaults.
   - Add tests matching existing naming and fixture conventions.
   - Run the most specific relevant test command directly.
   - Fix generated test failures if the fix is clear; stop after two failed attempts.

   **Docs**:
   - Determine documentation type and match existing style.
   - Write comments that explain why and intent, not obvious mechanics.
   - Avoid changelog-style comments.

   **ADR**:
   - Ask for missing Context, Decision, or Consequences if not provided.
   - Use the project's ADR directory and naming convention if one exists; otherwise propose `doc/adr/` or `docs/adr/`.

   **CI**:
   - Generate a minimal pipeline for the requested goal, typically lint/check → test → build → deploy only when required.
   - Check permissions, secrets, script injection risks, and cache keys.

5. Apply changes only within the requested scope.

6. Verify:
   - Tests mode: run generated tests.
   - Docs/ADR mode: check formatting or markdown lint if configured.
   - CI mode: run YAML/schema validation if available; otherwise explain manual verification steps.

## Output

1. **Mode**
2. **Conventions Detected**
3. **Generated Content / Files Changed**
4. **Verification Results**
5. **Remaining Follow-up**
