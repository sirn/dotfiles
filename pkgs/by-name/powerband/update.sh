#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl git nix cacert
set -euo pipefail

# Portable in-place sed (BSD sed requires -i extension, GNU sed does not)
sed_inplace() {
  local backup
  sed -i.bak "$@"
  # Find the last argument (the file) and remove its .bak
  backup="${@: -1}.bak"
  rm -f "$backup"
}

script_dir="$(dirname "${BASH_SOURCE[0]}")"
repo_root="$(cd "$script_dir/../../.." && pwd -P)"
package_file="$script_dir/package.nix"
pname="powerband"

if [ -n "${NIX_SSL_CERT_FILE:-}" ] && [ -f "${NIX_SSL_CERT_FILE}" ]; then
  export SSL_CERT_FILE="${NIX_SSL_CERT_FILE}"
fi

current_version=$(sed -n 's/.*version = "\([^"]*\)".*/\1/p' "$package_file" | head -1)

# Latest tag from the upstream GitHub repository
latest_tag=$(git ls-remote --tags "https://github.com/sirn/${pname}" |
  grep -v '\^{}' | awk -F/ '{print $NF}' | sort -V | tail -1)
version="${latest_tag#v}"

if [ "$current_version" = "$version" ]; then
  echo "Already at latest version: $version"
  exit 0
fi

echo "Updating from $current_version to $version"

# Prefetch the source tarball and compute its SRI hash
src_hash=$(nix hash convert --hash-algo sha256 --to sri \
  "$(nix-prefetch-url --unpack \
    "https://github.com/sirn/${pname}/archive/refs/tags/v${version}.tar.gz" \
    2>/dev/null | tail -1)")

# Update version and src hash in package.nix
sed_inplace \
  -e "s|version = \"$current_version\"|version = \"$version\"|" \
  -e "s|hash = \"sha256-[^\"]*\"|hash = \"$src_hash\"|" \
  "$package_file"

# Set a fake cargoHash so the cargo vendor staging derivation fails and reports
# the correct fixed-output hash. The vendor staging only downloads crates; it
# does not compile any Rust code.
sed_inplace 's|cargoHash = "sha256-[^"]*"|cargoHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA="|' "$package_file"

build_log=$(nix build --no-link "path:${repo_root}#${pname}.cargoDeps" 2>&1 || true)
cargo_hash=$(echo "$build_log" | grep 'got:' | head -1 | sed 's/.*got: *//')

if [ -z "$cargo_hash" ]; then
  echo "ERROR: Failed to determine cargoHash"
  echo "$build_log" | tail -20
  exit 1
fi

sed_inplace "s|cargoHash = \"sha256-[^\"]*\"|cargoHash = \"$cargo_hash\"|" "$package_file"

echo "Done. Updated to $version"
