NODE2NIX	:= nix run nixpkgs.nodePackages.node2nix -c node2nix

node-packages.nix: node-packages.json
	${NODE2NIX} -i $<
