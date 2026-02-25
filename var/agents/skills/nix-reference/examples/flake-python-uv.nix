{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [
            (final: prev: {
              wrapped-uv = prev.stdenv.mkDerivation {
                pname = "wrapped-uv";
                version = prev.uv.version;
                nativeBuildInputs = [ prev.makeWrapper ];
                dontUnpack = true;
                installPhase =
                  let
                    fhsUv = prev.buildFHSEnv {
                      name = "uv";
                      runScript = "uv";
                      targetPkgs = pkgs': with pkgs'; [
                        uv
                        openssl
                        pkg-config
                        gcc
                        zlib
                      ];

                      meta.mainProgram = "uv";
                    };

                    actualUv = if prev.stdenv.isLinux then fhsUv else prev.uv;
                  in
                  ''
                    mkdir -p $out/bin
                    makeWrapper ${actualUv}/bin/${actualUv.meta.mainProgram} $out/bin/uv --add-flags "--managed-python"
                    makeWrapper ${actualUv}/bin/${actualUv.meta.mainProgram} $out/bin/uvx --add-flags "--managed-python" --add-flags "tool run"
                  '';
              };

              # For compatibility if Poetry is needed
              wrapped-uv-poetry = prev.writeShellScriptBin "poetry" ''
                exec ${final.wrapped-uv}/bin/uvx poetry "$@"
              '';
            })
          ];
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.wrapped-uv
            pkgs.wrapped-uv-poetry
          ];
        };
      }
    );
}
