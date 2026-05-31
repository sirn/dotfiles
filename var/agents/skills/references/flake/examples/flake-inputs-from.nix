{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    repo1.url = "path:/path/to/repo1";
    repo2.url = "path:/path/to/repo2";
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      repo1,
      repo2,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
      in
      {
        devShells.default = pkgs.mkShell {
          inputsFrom = [
            repo1.devShells.${system}.default
            repo2.devShells.${system}.default
          ];
        };
      }
    );
}
