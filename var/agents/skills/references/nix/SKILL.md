---
name: nix
type: reference
description: Local Nix patterns, string escaping, shebang caveats, hash prefetching, and flake conventions. ALWAYS read BEFORE running nix commands or modifying flake.nix.
---

## Nix Conventions & Gotchas

Run `nix <subcommand> --help` (or `man nix3-<subcommand>`) for exact CLI syntax and flags. Apply these local conventions and gotchas:

### Rules & Package Lookup

- **Never use `nix-env -i`**: Prefer declarative configuration, `nix run`, `nix shell`, or `pkgs.mkShell`.
- **Find binaries**: Use `nix-locate bin/<command>` (from `nix-index`) or `nix search nixpkgs <query>`.
- **Inspect large configs cheaply**: Pass `--apply 'x: builtins.attrNames x'` to `nix eval` before serializing big attrsets.
- **Format flakes**: Run `nix run .#treefmt` (or `nix run .#treefmt -- --ci` to check).

### Script Shebangs

- **Modern `nix` shebang (CLI tools)**:

  ```bash
  #!/usr/bin/env nix
  #! nix shell nixpkgs#bash nixpkgs#curl nixpkgs#jaq --command bash
  ```

- **Python with packages (`nix-shell` shebang)**: `nix shell` only adds `bin/` to `PATH` and does not set `PYTHONPATH` for `python3Packages.*`. Use `python3.withPackages` with `nix-shell`:

  ```python
  #!/usr/bin/env nix-shell
  #! nix-shell -i python3 -p "python3.withPackages (ps: [ ps.requests ])"
  ```

### String Escaping in `'' ... ''`

| Desired Output | Nix Syntax | Notes |
| --- | --- | --- |
| `${variable}` | `''${variable}` | Escape Nix interpolation with `''` |
| `$${variable}` | `$''${variable}` | Literal dollar before variable |
| `''${literal}` | `'''${literal}` | Literal double-single-quote before `${...}` |
| `''` | `'''` | Literal double single quotes |

### Hash Prefetching & Fixed-Output Derivations

```bash
# Prefetch a URL (add --unpack for archives) and output its SRI sha256 hash
nix store prefetch-file --json https://example.com/archive.tar.gz | jaq -r .hash

# Convert hex/base32 sha256 to SRI format
nix hash convert --hash-algo sha256 --to sri <hash>
```

For Fixed-Output Derivations (`fetchFromGitHub`, `cargoHash`, `vendorHash`, `npmDepsHash`), set `hash = lib.fakeHash;`, run `nix build .#<pkg>`, and copy the `got: sha256-...` value from the error.

### Derivation & Shell Helpers

- **`pkgs.mkShell` vs `pkgs.mkShellNoCC`**: Use `mkShellNoCC` for pure scripting languages (Python, JS, Go, Rust with rustup) where a C toolchain is unnecessary; use `mkShell` when native C/C++ compilation is needed.
- **`pkgs.writeShellApplication`**: Preferred over `writeShellScriptBin` for Bash scripts — provides automatic `shellcheck` validation and explicit `runtimeInputs`.

### Templates & Examples

- Basic devShell flake: [examples/flake-basic.nix](examples/flake-basic.nix)
- Multi-repo devShell integration: [examples/flake-inputs-from.nix](examples/flake-inputs-from.nix)
- Python environment with uv: [examples/flake-python-uv.nix](examples/flake-python-uv.nix)
- Package overlay pattern: [examples/overlay-pattern.nix](examples/overlay-pattern.nix)
- Legacy `nix-shell` commands: [examples/interactive-shell.bash](examples/interactive-shell.bash)
