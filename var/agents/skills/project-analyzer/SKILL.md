---
name: project-analyzer
description: Analyzes project structure to identify tooling and workflows. Use during setup or environment detection.
---

Analyze project structure to identify tooling and workflows.

## Detection Areas
1. **Existing wrappers**: bin/, .my/bin/
2. **Build systems**: Makefile, Taskfile.yml, justfile
3. **Containers**: Dockerfile, Containerfile, docker-compose.yml, compose.yml
4. **Nix**: flake.nix, .my/flake.nix, shell.nix
5. **Package managers**: package.json, pyproject.toml, go.mod, Cargo.toml, Gemfile
6. **CI/CD**: .github/workflows/, .gitlab-ci.yml, Jenkinsfile

## Output
Return findings as:
- **Project type**: (nodejs, python, go, rust, etc.)
- **Existing wrappers**: paths found (bin/, .my/bin/, or none)
- **Has flake**: location if exists (flake.nix, .my/flake.nix, or none)
- **Recommended wrappers**: list with command for each:
  - test: command to run tests
  - lint: command to run linting
  - fmt: command to run formatting
  - build: command to build
  - dev: command to start dev server
- **Existing tools**: what's already set up (direnv, devenv, etc.)
