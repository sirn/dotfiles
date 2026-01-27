---
name: code-setup-project
description: Sets up project development environment (wrapper scripts and/or Nix flake). Use when user wants to set up a development environment, create wrapper scripts, or add a Nix flake.
---

Set up a project development environment with wrapper scripts and/or a Nix flake.

## Parameters
- **location**: Desired location for setup artifacts.
  - "machine-local": Inside `.my/` (ignored by git, good for personal tools).
  - "project-local": Root level (e.g., `bin/`, `flake.nix`).
- **setup_types**: What to set up ("wrapper", "flake", or "both").

If these parameters are not explicitly provided in the request, infer them from context or ask the user for clarification before proceeding.

## Process

For each setup type selected:

### Wrapper Scripts

1. Determine paths based on location:
   - Machine-local: `.my/bin/` with `my-` prefix
   - Project-local: `bin/`, ask about naming:
     - Generic: `test`, `lint`, `fmt`, `build`, `dev`
     - Prefixed (default): `<project-name>-test`, etc.

2. Invoke the `code-analyze-project` skill to:
   - Detect project type
   - Recommend wrappers (test, lint, fmt, build, dev commands)

3. Present recommended wrappers and ask which to create

4. Create wrapper directory if needed

5. If machine-local: Create `.my/.gitignore` with content `*`

6. Create wrapper scripts with template:
```bash
#!/usr/bin/env bash
set -euo pipefail
<command>
```

7. Make executable: `chmod +x <dir>/*`

### Nix Flake

1. Determine path based on location:
   - Machine-local: `.my/flake.nix`
   - Project-local: `flake.nix`

2. Check for existing flake at the determined path

3. Detect project type and required packages:
   - Node.js: package.json → nodejs, npm/pnpm/yarn
   - Python: pyproject.toml, setup.py → python3, pip/poetry/uv
   - Go: go.mod → go, gopls
   - Rust: Cargo.toml → cargo, rustc
   - Ruby: Gemfile → ruby, bundler
   - Other: check for Makefile, CMakeLists.txt, etc.

4. Generate flake using the template in [templates/flake.nix](templates/flake.nix)
   - Use `buildInputs` (not `packages`) for dependencies
   - No shellHook unless absolutely necessary
   - Keep it simple and minimal
   - If updating existing flake, preserve custom inputs/outputs but simplify structure

5. If machine-local: Create `.my/.gitignore` with content `*` if not exists

6. Verify the flake: `nix flake check path:.`
- If verification fails, fix issues and re-verify

## Output

- Project type detected
- Wrapper/flake location created
- Scripts/flake created (with descriptions)
- How to use
