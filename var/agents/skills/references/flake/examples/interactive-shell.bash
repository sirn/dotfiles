# Ad-hoc shell with packages
nix-shell -p curl jaq --run "curl -s https://api.example.com | jaq ."

# Enter interactive shell with packages
nix-shell -p python3 python3Packages.requests

# Pure shell (no host environment leakage)
nix-shell -p nodejs --pure

# Pin to specific nixpkgs version
nix-shell -p go -I nixpkgs=https://nixos.org/channels/nixos-26.05/nixexprs.tar.xz
