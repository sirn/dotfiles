---
name: nix-reference
description: Reference for Nix commands, flake patterns, and best practices
---

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
