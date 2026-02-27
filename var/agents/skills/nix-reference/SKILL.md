---
name: nix-reference
type: reference
description: Reference for Nix commands, flake patterns, and best practices
---

## String Escaping

When writing Nix strings that contain code for other languages (TypeScript, JavaScript, etc.), remember:

| What you want in output | Nix syntax |
|------------------------|-----------|
| `${variable}` | `''${variable}` |
| `$${variable}` | `$''${variable}` |
| `''${literal}` | `'''${literal}` |

**Rule**: Use two single quotes `''` before `${}` to prevent Nix from interpolating it.

**Example** - Generating TypeScript with template literals:

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

### Flake Commands
- `nix build .#<package>` - Build a package
- `nix run .#<package>` - Run a package
- `nix develop` - Enter dev shell
- `nix flake check` - Validate flake
- `nix flake update` - Update flake.lock

### Interactive nix-shell

See [examples/interactive-shell.bash](examples/interactive-shell.bash)

### nix-shell Shebang Patterns

> **Note**: Update the nixpkgs channel URL (e.g., `nixos-25.11`) to match your current NixOS release.

**Finding your current release:**
- On NixOS: `nixos-version` (shows current system version, e.g., `25.05.20250224...`)
- From flake.lock: Check the `nixpkgs` input revision or run `nix flake metadata` to see locked references
- Check system flake: `cat /etc/nixos/flake.nix | grep -E "nixos-24|nixos-25"` or similar
- Use `channels` command: `nix-channel --list` (if using channels instead of flakes)

#### Bash script
```bash
#!/usr/bin/env nix-shell
#! nix-shell -i bash --pure
#! nix-shell -p bash curl jq
#! nix-shell -I nixpkgs=https://nixos.org/channels/nixos-25.11/nixexprs.tar.xz

curl -s https://api.example.com | jq .
```

#### Python script
```python
#!/usr/bin/env nix-shell
#! nix-shell -i python3 --pure
#! nix-shell -p python3 python3Packages.requests
#! nix-shell -I nixpkgs=https://nixos.org/channels/nixos-25.11/nixexprs.tar.xz

import requests
print(requests.get("https://api.example.com").json())
```

### devShell Patterns

#### mkShell vs mkShellNoCC
- `mkShell` - When you need C compiler (native extensions)
- `mkShellNoCC` - Pure scripting (Python, Node.js, Go)

#### Basic flake template (recommended)

See [examples/flake-basic.nix](examples/flake-basic.nix)

#### Python with uv (recommended)

See [examples/flake-python-uv.nix](examples/flake-python-uv.nix)

### Overlay Pattern

See [examples/overlay-pattern.nix](examples/overlay-pattern.nix)

### Never Use
- `nix-env -i` (use flakes or declarative config)
