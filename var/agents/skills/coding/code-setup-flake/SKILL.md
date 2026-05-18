---
name: code-setup-flake
description: Create or update a minimal Nix flake for project development. Use when asked to add a flake, update devShells, or set up Nix-based development.
---

Create or update a minimal Nix flake for a project.

## Prerequisites

Reference these skills first:

- Read `nix` skill for nix-shell, package lookup, and Nix string escaping.
- Read `flake` skill for `path:.` usage, devShell patterns, and flake templates.

## Parameters

Determine or ask for:

- **location**: `machine-local` (`.my/flake.nix`, ignored by git) or `project-local` (`flake.nix`).

## Process

1. Spawn applicable agents in parallel:
   - `scout`: "Detect project tooling, package managers, existing Nix files, wrappers, task runners, and setup conventions."
   - `researcher`: "Find official Nix/flake guidance and exact package recommendations for this project type."
   - `reviewer`: "Audit the proposed flake with a simplicity lens: unnecessary packages, boilerplate, shell hooks, extra inputs, and over-engineering."

2. Inspect project setup yourself:
   - Nix: `flake.nix`, `.my/flake.nix`, `shell.nix`, `default.nix`.
   - Package managers: `package.json`, `pyproject.toml`, `go.mod`, `Cargo.toml`, `Gemfile`.
   - Task runners and wrappers: `Makefile`, `Taskfile.yml`, `justfile`, `bin/`, `.my/bin/`.
   - Existing project instructions: `README.md`, `CONTRIBUTING.md`, `AGENTS.md`, `GEMINI.md`, `CODEX.md`.

3. Determine path:
   - Machine-local: `.my/flake.nix`.
   - Project-local: `flake.nix`.

4. Check for an existing flake first.
5. Detect required packages from project files.
6. Generate or update a minimal flake using `templates/flake.nix` when appropriate.
7. Use `buildInputs` for shell dependencies unless the project pattern says otherwise.
8. Avoid shell hooks and extra inputs unless strictly necessary.
9. Verify with `nix flake check path:.` or the correct `path:` form for the flake directory.

## Output

1. **Project Setup Detected**
2. **Flake Changes**
3. **Files Modified**
4. **Simplicity Audit Summary**
5. **Verification Status**
6. **How to Use**
