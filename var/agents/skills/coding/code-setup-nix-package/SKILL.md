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

1. Spawn applicable agents in parallel:
   - `scout`: "Locate existing Nix environment files and package ordering/style conventions."
   - `researcher`: "Find and verify the exact nixpkgs attribute for the requested package using authoritative sources."
   - `reviewer`: "Audit the proposed package addition for necessity, correct environment placement, and avoidable complexity."

2. Identify the package requested by the user.
3. Verify the exact nixpkgs attribute with `nix-locate`, `nix search`, WebSearch/WebFetch, or official nixpkgs references.
4. Locate the appropriate environment file: `flake.nix`, `.my/flake.nix`, `shell.nix`, or `default.nix`.
5. Add the package while preserving comments, indentation, and ordering style.
6. Verify with the appropriate Nix command using `path:` for flakes.
7. If verification fails, stop and report the issue; do not guess another package name.

## Output

1. **Package Requested**
2. **Verified nixpkgs Attribute**
3. **Files Modified**
4. **Simplicity Audit Summary**
5. **Verification Status**
6. **How to Use**
