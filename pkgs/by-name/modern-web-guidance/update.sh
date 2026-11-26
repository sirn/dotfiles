#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages curl jq nix cacert
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
sources_file="$script_dir/sources.json"

if [ -n "${NIX_SSL_CERT_FILE:-}" ] && [ -f "${NIX_SSL_CERT_FILE:-}" ]; then
	export SSL_CERT_FILE="${NIX_SSL_CERT_FILE}"
fi

latest=$(curl -sL https://registry.npmjs.org/modern-web-guidance/latest)
version=$(jq -r '.version' <<<"$latest")
url=$(jq -r '.dist.tarball' <<<"$latest")

current_version=$(jq -r '.version' "$sources_file")
if [ "$current_version" = "$version" ]; then
	echo "Already at latest version: $version"
	exit 0
fi

echo "Updating to ${version}"

src_hash=$(nix hash convert --hash-algo sha256 --to sri \
	"$(nix-prefetch-url "$url" 2>/dev/null | tail -1)")

jq -n \
	--arg version "$version" \
	--arg url "$url" \
	--arg srcHash "$src_hash" \
	'{version: $version, url: $url, srcHash: $srcHash}' >"$sources_file"

echo "Done. Updated to $version"
