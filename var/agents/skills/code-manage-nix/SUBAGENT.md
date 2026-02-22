---
name: code-manage-nix
description: Search for and add Nix packages to the project. Use when asked to add a package or dependency in Nix.
---

Search for and add Nix packages to the environment.

## Process
1. **Identify Package**:
   - Get the common name from user (e.g., "ripgrep", "python requests")
   - Spawn `code-researcher` (or use `nix-locate`/`web-search`) to find the **exact attribute path** in `nixpkgs` (e.g., `pkgs.ripgrep`, `pkgs.python3Packages.requests`)
   - *Crucial*: Verify the package exists in the current nixpkgs channel/version

2. **Detect Configuration**:
   - Locate `flake.nix`, `shell.nix`, or `default.nix`
   - Identify where `buildInputs` or `packages` are defined

3. **Modify Configuration**:
   - Add the verified package attribute to the list
   - Maintain alphabetical order if existing list is sorted
   - Use `write_file` or `replace` to apply changes

4. **Verify**:
   - Run `nix flake check` (if flake) or `nix-shell --run "true"`
   - If verification fails, undo and report

## Output
1. **Package Found**: Exact attribute name
2. **File Modified**: Path to config
3. **Verification Status**: Success/Fail
