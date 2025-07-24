{ lib, pkgs, ... }:

let
  fhsUvx = pkgs.buildFHSEnv {
    name = "uvx-fhs-base";
    runScript = "uv";
    targetPkgs = pkgs': with pkgs'; [
      uv

      # Used by uv
      openssl
      pkg-config
      stdenv.cc.cc
      zlib

      # For dynamically linked binaries
      nix-ld
    ];
  };

  wrappedFhsUv = pkgs.stdenv.mkDerivation {
    pname = "wrapped-uv";
    src = ./.;
    version = pkgs.uv.version;

    nativeBuildInputs = [
      pkgs.makeWrapper
    ];

    installPhase = ''
      mkdir -p $out/bin

      makeWrapper ${fhsUvx}/bin/${fhsUvx.name} $out/bin/uv
      makeWrapper ${fhsUvx}/bin/${fhsUvx.name} $out/bin/uvx \
        --add-flags "tool uvx" \
    '';
  };
in

if pkgs.stdenv.isLinux
then wrappedFhsUv
else pkgs.uv
