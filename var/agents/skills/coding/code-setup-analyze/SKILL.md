---
name: code-setup-analyze
description: Analyze project tooling and development workflows without changing files. Use when asked to inspect project setup, detect tooling, or summarize available commands.
---

Analyze a project development environment. This skill is read-only by default.

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

1. Read project instructions and repository structure.
2. Detect project type, package managers, task runners, wrappers, containers, Nix files, and CI configuration.
3. Identify existing commands for test, lint/check, format, build, and development.
4. Identify setup gaps or duplication, but do not change files unless the user explicitly asks for a follow-up implementation.

## Output

1. **Project Type**
2. **Existing Wrappers**
3. **Flake / Shell Status**
4. **Package Manager and Task Runner**
5. **Available Commands** for test, lint/check, format, build, dev
6. **Recommended Minimal Setup Changes**
7. **Complexity / Over-Engineering Risks**
