#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages curl jq nix cacert
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
sources_file="$script_dir/sources.json"

if [ -n "${NIX_SSL_CERT_FILE:-}" ] && [ -f "${NIX_SSL_CERT_FILE:-}" ]; then
	export SSL_CERT_FILE="${NIX_SSL_CERT_FILE}"
fi

releases=$(curl -sL "https://api.github.com/repos/pbakaus/impeccable/releases?per_page=30")
tag=$(jq -r '[.[] | select(.tag_name | startswith("skill-v"))][0].tag_name' <<<"$releases")
version="${tag#skill-v}"

current_version=$(jq -r '.version' "$sources_file")
if [ "$current_version" = "$version" ]; then
	echo "Already at latest version: $version"
	exit 0
fi

# Resolve the commit SHA the tag points at (handles both lightweight and annotated tags)
ref=$(curl -sL "https://api.github.com/repos/pbakaus/impeccable/git/refs/tags/${tag}")
ref_type=$(jq -r '.object.type' <<<"$ref")
if [ "$ref_type" = "tag" ]; then
	rev=$(curl -sL "https://api.github.com/repos/pbakaus/impeccable/git/tags/$(jq -r '.object.sha' <<<"$ref")" | jq -r '.object.sha')
else
	rev=$(jq -r '.object.sha' <<<"$ref")
fi

echo "Updating to ${version} (${rev})"

src_hash=$(nix hash convert --hash-algo sha256 --to sri \
	"$(nix-prefetch-url --unpack "https://github.com/pbakaus/impeccable/archive/${rev}.tar.gz" 2>/dev/null | tail -1)")

raw_base="https://raw.githubusercontent.com/pbakaus/impeccable/${rev}/.agents/skills/impeccable/scripts"
engine_version=$(curl -fsSL "${raw_base}/VERSION" | tr -d '[:space:]')
checksums=$(curl -fsSL "${raw_base}/engine-checksums.txt")

engine_sri() {
	local asset="$1"
	local hex
	hex=$(awk -v ver="$engine_version" -v asset="$asset" '$1 == ver && $2 == asset { print $3; exit }' <<<"$checksums")
	if [ -z "$hex" ]; then
		echo "ERROR: Missing checksum for ${asset} (${engine_version})" >&2
		exit 1
	fi
	nix hash convert --hash-algo sha256 --from base16 --to sri "$hex"
}

h_darwin_arm64=$(engine_sri "impeccable-darwin-arm64")
h_darwin_x64=$(engine_sri "impeccable-darwin-x64")
h_linux_arm64=$(engine_sri "impeccable-linux-arm64")
h_linux_x64=$(engine_sri "impeccable-linux-x64")

jq -n \
	--arg version "$version" \
	--arg rev "$rev" \
	--arg srcHash "$src_hash" \
	--arg engineVersion "$engine_version" \
	--arg aarch64Darwin "$h_darwin_arm64" \
	--arg x8664Darwin "$h_darwin_x64" \
	--arg aarch64Linux "$h_linux_arm64" \
	--arg x8664Linux "$h_linux_x64" \
	'{
    version: $version,
    rev: $rev,
    srcHash: $srcHash,
    engineVersion: $engineVersion,
    engineHashes: {
      "aarch64-darwin": $aarch64Darwin,
      "x86_64-darwin": $x8664Darwin,
      "aarch64-linux": $aarch64Linux,
      "x86_64-linux": $x8664Linux
    }
  }' >"$sources_file"

echo "Done. Updated to $version (engine $engine_version, $rev)"
