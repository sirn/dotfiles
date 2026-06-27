---
name: nix
type: reference
description: Reference for Nix commands, nix-shell patterns, package lookup, and Nix string escaping. ALWAYS read BEFORE running nix commands that are not specifically flake operations.
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

See also [flake](../flake/SKILL.md) for flake-specific commands and templates, or inspect [examples/interactive-shell.bash](examples/interactive-shell.bash).

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
