---
name: nix
type: reference
description: Reference for Nix commands, nix-shell patterns, package lookup, Nix string escaping, and Nix flakes (flake commands, devShell patterns, templates, path:. usage). ALWAYS read BEFORE running nix commands or modifying flake.nix.
---

## String Escaping

When writing Nix strings containing code for other languages (such as TypeScript or JavaScript):

| What you want in output | Nix syntax       |
| ----------------------- | ---------------- |
| `${variable}`           | `''${variable}`  |
| `$${variable}`          | `$''${variable}` |
| `''${literal}`          | `'''${literal}`  |

**Rule**: Use two single quotes `''` before `${}` to prevent Nix interpolation.

**Example**: Generating TypeScript with template literals.

Nix source:

```nix
{
  xdg.configFile."my-plugin.ts".text = ''
    function log(msg: string) {
      console.log(`[''${timestamp}] ''${msg}`);
    }
  '';
}
```

Generated TypeScript:

```typescript
function log(msg: string) {
  console.log(`[${timestamp}] ${msg}`);
}
```

## Nix Command Reference

### Interactive nix-shell

Use `nix-shell -p` for ad-hoc tools and temporary environments.

Inspect [examples/interactive-shell.bash](examples/interactive-shell.bash).

### nix-shell Shebang Patterns

> **Note**: Update the nixpkgs channel URL (e.g., `nixos-26.05`) to match your current NixOS release.

**Finding your current release:**

- On NixOS: Run `nixos-version` to view the active system version.
- From flakes: Check the `nixpkgs` revision in `flake.lock`, run `nix flake metadata` to view locked references, or inspect `/etc/nixos/flake.nix` with `cat /etc/nixos/flake.nix | grep -E "nixos-24|nixos-25"`.
- From legacy channels: Run `nix-channel --list`.

#### Bash script

```bash
#!/usr/bin/env nix-shell
#! nix-shell -i bash --pure
#! nix-shell -p bash curl jq
#! nix-shell -I nixpkgs=https://nixos.org/channels/nixos-26.05/nixexprs.tar.xz

curl -s https://api.example.com | jq .
```

#### Python script

```python
#!/usr/bin/env nix-shell
#! nix-shell -i python3 --pure
#! nix-shell -p python3 python3Packages.requests
#! nix-shell -I nixpkgs=https://nixos.org/channels/nixos-26.05/nixexprs.tar.xz

import requests
print(requests.get("https://api.example.com").json())
```

## Package Lookup

- Verify exact package names with `nix-locate`, `nix search`, WebFetch, or WebSearch rather than guessing Nix attribute paths.
- Never use `nix-env -i`; prefer flakes, profiles, or declarative configuration.

## Flakes

### Flake Command Reference
- `nix build path:.#<package>` - Build a package
- `nix run path:.#<package>` - Run a package
- `nix develop path:.` - Enter dev shell
- `nix flake check path:.` - Validate flake
- `nix flake update` - Update flake.lock

In a dirty workspace, use `path:.` or `path:/path/to/flake/dir` so untracked files are recognized.

### devShell Patterns

#### mkShell vs mkShellNoCC
- `mkShell` - For C compilers and native extensions
- `mkShellNoCC` - For pure scripting (Python, Node.js, Go)

#### Templates
- Basic flake: [examples/flake-basic.nix](examples/flake-basic.nix)
- inputsFrom (workspace combining devShells): [examples/flake-inputs-from.nix](examples/flake-inputs-from.nix)
- Python with uv/poetry: [examples/flake-python-uv.nix](examples/flake-python-uv.nix)
- Overlay pattern: [examples/overlay-pattern.nix](examples/overlay-pattern.nix)

### Formatting

The project formatter is treefmt. Check formatting with `nix run path:.#treefmt -- --ci`; apply with `nix run path:.#treefmt`.