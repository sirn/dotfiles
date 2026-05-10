---
name: code-setup-nix-package
description: Add a verified nixpkgs package to a Nix development environment. Use when asked to add a package to flake.nix, shell.nix, default.nix, or a devShell.
---

Find a verified nixpkgs attribute and add it to the appropriate Nix environment.

## Prerequisites

Reference these skills first:

- Read `nix` skill for nix-shell, package lookup, and Nix string escaping.
- Read `flake` skill for `path:.` usage and flake command patterns when flakes are involved.

## Process

1. Identify the package requested by the user.
2. Verify the exact nixpkgs attribute with `nix-locate`, `nix search`, WebSearch/WebFetch, or official nixpkgs references.
3. Locate the appropriate environment file: `flake.nix`, `.my/flake.nix`, `shell.nix`, or `default.nix`.
4. Add the package while preserving comments, indentation, and ordering style.
5. Verify with the appropriate Nix command using `path:` for flakes.
6. If verification fails, stop and report the issue; do not guess another package name.

## Output

1. **Package Requested**
2. **Verified nixpkgs Attribute**
3. **Files Modified**
4. **Verification Status**
5. **How to Use**
