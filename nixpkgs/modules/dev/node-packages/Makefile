node-packages.nix: node-packages.json
	nix-shell -p nodePackages.node2nix --run "node2nix --input $<"

PHONY: clean
clean:
	rm default.nix
	rm node-env.nix
	rm node-packages.nix
