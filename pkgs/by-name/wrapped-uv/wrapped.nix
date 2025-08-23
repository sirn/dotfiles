{ lib, pkgs, ... }:

let
  fhsUv = pkgs.buildFHSEnv {
    name = "uvx-fhs-base";
    runScript = "uv";
    targetPkgs = pkgs': with pkgs'; [
      uv

      # Used by uv
      openssl
      pkg-config
      stdenv.cc.cc
      zlib
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

      makeWrapper ${fhsUv}/bin/${fhsUv.name} $out/bin/uv
      makeWrapper ${fhsUv}/bin/${fhsUv.name} $out/bin/uvx \
        --add-flags "tool uvx" \
    '';
  };
in

if pkgs.stdenv.isLinux
then wrappedFhsUv
else pkgs.uv
