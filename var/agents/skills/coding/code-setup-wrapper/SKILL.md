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

1. Inspect project setup:
   - Existing wrappers: `bin/`, `.my/bin/`.
   - Task runners: `Makefile`, `Taskfile.yml`, `justfile`.
   - Package managers: `package.json`, `pyproject.toml`, `go.mod`, `Cargo.toml`, `Gemfile`.
   - Existing project instructions: `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md`.

2. Determine paths:
   - Machine-local: `.my/bin/` with `my-` prefix when useful.
   - Project-local: `bin/`, with project-appropriate names.

3. Recommend only wrappers that are useful for this project.
4. Ask which wrappers to create unless the user already specified them.
5. If machine-local, ensure `.my/.gitignore` contains `*`.
6. Create scripts using:

```bash
#!/usr/bin/env bash
set -euo pipefail
<command>
```

7. Make scripts executable.
8. Verify basic execution where safe.

## Output

1. **Project Setup Detected**
2. **Wrappers Created**
3. **Files Modified**
4. **Verification Status**
5. **How to Use**
