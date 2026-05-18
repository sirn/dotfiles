---
name: code-setup-wrapper
description: Create minimal development wrapper scripts. Use when asked to add scripts for common test, lint, format, build, dev, or project commands.
---

Create useful wrapper scripts for project development workflows.

## Parameters

Determine or ask for:

- **location**: `machine-local` (`.my/`, ignored by git) or `project-local` (repository files such as `bin/`).
- **commands**: the specific workflows to wrap, such as test, lint/check, format, build, or dev.

## Process

1. Spawn applicable agents in parallel:
   - `scout`: "Detect existing wrappers, task runners, package managers, setup conventions, and available project commands."
   - `reviewer`: "Audit the proposed wrapper scripts with a simplicity lens: unnecessary wrappers, duplicate commands, unsafe shell, and over-engineering."

2. Inspect project setup yourself:
   - Existing wrappers: `bin/`, `.my/bin/`.
   - Task runners: `Makefile`, `Taskfile.yml`, `justfile`.
   - Package managers: `package.json`, `pyproject.toml`, `go.mod`, `Cargo.toml`, `Gemfile`.
   - Existing project instructions: `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md`.

3. Determine paths:
   - Machine-local: `.my/bin/` with `my-` prefix when useful.
   - Project-local: `bin/`, with project-appropriate names.

4. Recommend only wrappers that are useful for this project.
5. Ask which wrappers to create unless the user already specified them.
6. If machine-local, ensure `.my/.gitignore` contains `*`.
7. Create scripts using:

```bash
#!/usr/bin/env bash
set -euo pipefail
<command>
```

8. Make scripts executable.
9. Verify basic execution where safe.

## Output

1. **Project Setup Detected**
2. **Wrappers Created**
3. **Files Modified**
4. **Simplicity Audit Summary**
5. **Verification Status**
6. **How to Use**
