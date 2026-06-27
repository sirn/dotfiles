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

In a dirty workspace, use `path:.` or `path:/path/to/flake/dir` so untracked files are recognized.

## devShell Patterns

### mkShell vs mkShellNoCC

- `mkShell` - For C compilers and native extensions
- `mkShellNoCC` - For pure scripting (Python, Node.js, Go)

### Basic flake template

Use [examples/flake-basic.nix](examples/flake-basic.nix) for a basic Flake setup.

### inputsFrom flake template

Use [examples/flake-inputs-from.nix](examples/flake-inputs-from.nix) when working in a workspace and combining project-specific devShells.

### Python with uv

Use [examples/flake-python-uv.nix](examples/flake-python-uv.nix) when you need `uv` and/or `poetry`.

### Overlay Pattern

See [examples/overlay-pattern.nix](examples/overlay-pattern.nix).

## Related

- Read [nix](../nix/SKILL.md) for nix-shell, package lookup, and Nix string escaping.
