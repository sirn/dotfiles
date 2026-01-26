# Ad-hoc shell with packages
nix-shell -p curl jq --run "curl -s https://api.example.com | jq ."

# Enter interactive shell with packages
nix-shell -p python3 python3Packages.requests

# Pure shell (no host environment leakage)
nix-shell -p nodejs --pure

# Pin to specific nixpkgs version
nix-shell -p go -I nixpkgs=https://nixos.org/channels/nixos-25.11/nixexprs.tar.xz
