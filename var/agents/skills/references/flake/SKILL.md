---
name: flake
type: reference
description: Reference for Nix flakes, flake commands, devShell patterns, templates, and path:. usage. ALWAYS read BEFORE running nix flake commands or modifying flake.nix.
---

## Flake Command Reference

- `nix build path:.#<package>` - Build a package
- `nix run path:.#<package>` - Run a package
- `nix develop path:.` - Enter dev shell
- `nix flake check path:.` - Validate flake
- `nix flake update` - Update flake.lock

When developing with flakes in a dirty workspace, use `path:.` or `path:/path/to/flake/dir` so untracked files are recognized.

## devShell Patterns

### mkShell vs mkShellNoCC

- `mkShell` - When you need C compiler (native extensions)
- `mkShellNoCC` - Pure scripting (Python, Node.js, Go)

### Basic flake template

See [examples/flake-basic.nix](examples/flake-basic.nix)

Use this template for basic Flake setup.

### inputsFrom flake template

See [examples/flake-inputs-from.nix](examples/flake-inputs-from.nix)

Use this template when you're working in a workspace and needs to combine project-specific devShells.

### Python with uv

See [examples/flake-python-uv.nix](examples/flake-python-uv.nix)

Use this template when you need `uv` and/or `poetry`.

### Overlay Pattern

See [examples/overlay-pattern.nix](examples/overlay-pattern.nix)

## Related

- Read [nix](../nix/SKILL.md) for nix-shell, package lookup, and Nix string escaping.
