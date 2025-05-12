# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build/Test Commands

- Apply changes: `home-manager switch --flake path:~/.dotfiles#<profile>`
- Test changes without applying: `home-manager build --no-out-link --flake path:~/.dotfiles#<profile>`
- Check repository diff: `jj diff`
- Check repository status: `jj status`
- Squash commit: `jj squash -m $(jj log --no-graph -r @- -T 'self.description()')`
- Reference previous commit: Use `-r @-`
- Reference current commit: Use `-r @`
- Create a new module: Create file in `~/.dotfiles/modules/programs/<name>.nix`

## Code Style Guidelines

### Nix

- Use 2-space indentation in .nix files
- Use lowercase for variable names (e.g., `overlays`, `config`)
- Use camelCase for function names (e.g., `mkConfig`, `mkLinuxConfig`)
- Follow existing imports structure in modules
- When modifying git settings, update modules/programs/git.nix
- When adding new packages, use home.packages array in respective module
- Use lib.mkIf for conditional configuration
- For new machines, add entry to homeConfigurations in flake.nix

### Shell Scripts

- Use POSIX-compatible shell script syntax (/bin/sh)
- Include header comments describing script purpose
- Follow shellcheck guidelines (avoid SC2034 where necessary)
- Use existing color and printing functions (printe_h1, printe_info, etc.)
