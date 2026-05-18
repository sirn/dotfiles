---
name: code-generate-docs
description: Generate or update documentation. Use when asked to add comments, docstrings, module docs, README sections, or reference documentation.
---

Generate documentation that matches existing project conventions.

## Process

1. Identify context:
   - If code changes are involved: run `jj diff -s` first to see changed files; then use `jj diff -- path` to restrict to specific files/directories.
   - If the user specified files, modules, audience, or requirements, focus on those.
   - Determine the documentation type: inline comments, docstrings, module/file docs, README content, or reference material.

2. Spawn applicable agents in parallel:
   - `scout`: "Identify existing documentation style, naming, structure, examples, and relevant code context for {files or requirements}."
   - `planner`: "Draft the minimal documentation structure needed to explain the behavior, intent, data flow, or tradeoffs."
   - `reviewer`: "Review the proposed documentation for clarity, convention fit, accuracy, and unnecessary detail."

3. Inspect existing conventions yourself:
   - Read relevant code and neighboring documentation.
   - Check existing documentation for naming, layout, heading style, examples, and detail level.
   - Check project instructions: `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md`.

4. Generate documentation:
   - Match existing documentation style and structure.
   - Write comments that explain why and intent, not obvious mechanics.
   - Prefer concise examples that can be verified from the current code.
   - Avoid changelog-style comments.

5. Apply changes only within the requested scope.

6. Verify:
   - Run configured formatting, markdown lint, docs build, or link checks when available and relevant.
   - Otherwise, review generated docs for accuracy against the current code.

## Output

1. **Documentation Scope**
2. **Conventions Detected**
3. **Documentation Added / Files Changed**
4. **Verification Results**
5. **Remaining Follow-up**
