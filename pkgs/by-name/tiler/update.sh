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
package_file="$script_dir/package.nix"
pname="tiler"

if [ -n "${NIX_SSL_CERT_FILE:-}" ] && [ -f "${NIX_SSL_CERT_FILE}" ]; then
  export SSL_CERT_FILE="${NIX_SSL_CERT_FILE}"
fi

current_version=$(sed -n 's/.*version = "\([^"]*\)".*/\1/p' "$package_file" | head -1)

# Latest tag from the upstream SourceHut git repository
latest_tag=$(git ls-remote --tags "https://git.sr.ht/~sirn/${pname}" |
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
    "https://git.sr.ht/~sirn/${pname}/archive/v${version}.tar.gz" \
    2>/dev/null | tail -1)")

# Update version and src hash in package.nix
sed_inplace \
  -e "s|version = \"$current_version\"|version = \"$version\"|" \
  -e "s|hash = \"sha256-[^\"]*\"|hash = \"$src_hash\"|" \
  "$package_file"

echo "Done. Updated to $version"
