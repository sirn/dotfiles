---
name: code-setup
description: Analyze project tooling and set up development helpers such as wrapper scripts, Nix flakes, or Nix packages. Use when asked to inspect project setup, create wrappers, add a flake, or add a Nix package.
---

Set up or analyze a project development environment.

## Modes

- **Analyze**: Detect project type, tooling, workflows, wrappers, and package managers.
- **Wrappers**: Create minimal wrapper scripts.
- **Flake**: Create or update a Nix flake.
- **Add Nix Package**: Find a verified nixpkgs attribute and add it to the environment.

## Parameters

For setup modes, determine or ask for:

- **location**: `machine-local` (`.my/`, ignored by git) or `project-local` (repository files such as `bin/` or `flake.nix`).
- **setup_types**: `wrapper`, `flake`, `add-nix-pkg`, or a combination.

## Project Detection

Inspect, as relevant:

1. Existing wrappers: `bin/`, `.my/bin/`.
2. Task runners: `Makefile`, `Taskfile.yml`, `justfile`.
3. Containers: `Dockerfile`, `Containerfile`, `docker-compose.yml`, `compose.yml`.
4. Nix: `flake.nix`, `.my/flake.nix`, `shell.nix`, `default.nix`.
5. Package managers: `package.json`, `pyproject.toml`, `go.mod`, `Cargo.toml`, `Gemfile`.
6. CI/CD: `.github/workflows/`, `.gitlab-ci.yml`, `Jenkinsfile`.
7. Existing project instructions: `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md`.

During detection, identify complexity hotspots, unnecessary tooling, and native alternatives before recommending new files.

## Process

### Analyze

Return:

- Project type
- Existing wrappers
- Flake/shell status
- Package manager and task runner
- Existing commands for test, lint/check, format, build, dev
- Recommended minimal setup changes

### Wrapper Scripts

1. Determine paths:
   - Machine-local: `.my/bin/` with `my-` prefix when useful.
   - Project-local: `bin/`, with project-appropriate names.
2. Recommend only wrappers that are useful for this project.
3. Ask which wrappers to create unless the user already specified them.
4. If machine-local, ensure `.my/.gitignore` contains `*`.
5. Create scripts using:

```bash
#!/usr/bin/env bash
set -euo pipefail
<command>
```

6. Make scripts executable.
7. Verify basic execution where safe.

### Nix Flake

1. Determine path:
   - Machine-local: `.my/flake.nix`.
   - Project-local: `flake.nix`.
2. Check for an existing flake first.
3. Detect required packages from project files.
4. Generate or update a minimal flake using `templates/flake.nix` when appropriate.
5. Use `buildInputs` for shell dependencies unless the project pattern says otherwise.
6. Avoid shell hooks and extra inputs unless strictly necessary.
7. Verify with `nix flake check path:.` or the correct `path:` form for the flake directory.

### Add Nix Package

1. Identify the package requested by the user.
2. Verify the exact nixpkgs attribute with `nix-locate`, `nix search`, WebSearch/WebFetch, or official nixpkgs references.
3. Locate the appropriate environment file: `flake.nix`, `.my/flake.nix`, `shell.nix`, or `default.nix`.
4. Add the package while preserving comments, indentation, and ordering style.
5. Verify with the appropriate Nix command using `path:` for flakes.
6. If verification fails, stop and report the issue; do not guess another package name.

## Output

1. **Mode**
2. **Project Setup Detected**
3. **Changes Recommended or Applied**
4. **Files Modified**
5. **Verification Status**
6. **How to Use**
