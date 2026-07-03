#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl jq nix cacert
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
package_file="$script_dir/default.nix"
owner="nikosdion"
repo="asdcontrol"
branch="main"

if [ -n "${NIX_SSL_CERT_FILE:-}" ] && [ -f "${NIX_SSL_CERT_FILE}" ]; then
  export SSL_CERT_FILE="${NIX_SSL_CERT_FILE}"
fi

current_rev=$(sed -n 's/.*rev = "\([^"]*\)".*/\1/p' "$package_file" | head -1)

# Latest commit on the tracked branch
commit_json=$(curl -sL "https://api.github.com/repos/${owner}/${repo}/commits/${branch}")
rev=$(jq -r '.sha' <<<"$commit_json")
version=$(jq -r '.commit.author.date | split("T")[0] | gsub("-"; "")' <<<"$commit_json")

if [ "$current_rev" = "$rev" ]; then
  echo "Already at latest revision: $rev"
  exit 0
fi

echo "Updating from $current_rev to $rev"

# Prefetch the source tarball and compute its SRI hash
src_hash=$(nix hash convert --hash-algo sha256 --to sri \
  "$(nix-prefetch-url --unpack \
    "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz" \
    2>/dev/null | tail -1)")

# Update version, rev and src hash in default.nix
sed_inplace \
  -e "s|version = \"[^\"]*\"|version = \"$version\"|" \
  -e "s|rev = \"[^\"]*\"|rev = \"$rev\"|" \
  -e "s|sha256 = \"sha256-[^\"]*\"|sha256 = \"$src_hash\"|" \
  "$package_file"

echo "Done. Updated to $version ($rev)"
