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
                      name = "uv-fhs";
                      runScript = "uv";
                      targetPkgs = pkgs': with pkgs'; [
                        prev.uv
                        openssl
                        pkg-config
                        prev.stdenv.cc.cc
                        zlib
                      ];
                    };
                    actualUv = if prev.stdenv.isLinux then fhsUv else prev.uv;
                  in
                  ''
                    mkdir -p $out/bin
                    makeWrapper ${actualUv}/bin/uv $out/bin/uv
                    makeWrapper ${actualUv}/bin/uv $out/bin/uvx --add-flags "tool run"
                  '';
              };
            })
          ];
        };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = [ pkgs.wrapped-uv ];
        };
      }
    );
}
