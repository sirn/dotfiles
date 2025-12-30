#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages uv python3 python3Packages.requests nix-update git gnused
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[@]}")"
current_version=$(sed -n 's/^  name = "mcp-server-fetch-\(.*\)";$/\1/p' "$script_dir/package.nix")

get_pypi_version() {
  python3 -c "import requests; print(requests.get('https://pypi.org/pypi/mcp-server-fetch/json').json()['info']['version'])"
}

version=$(get_pypi_version)

if [ "$current_version" = "$version" ]; then
  exit 0
fi

cd "$script_dir/uv2nix"
sed "s/\"mcp-server-fetch==.*\"/\"mcp-server-fetch==$version\"/" pyproject.toml > pyproject.toml.tmp
mv pyproject.toml.tmp pyproject.toml
uv lock --upgrade

cd "$script_dir"
sed -i "s/^  name = \"mcp-server-fetch-.*\";/  name = \"mcp-server-fetch-$version\";/" package.nix
