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

    meta.mainProgram = "uv";
  };

  actualUv =
    if pkgs.stdenv.isLinux
    then fhsUv
    else pkgs.uv;
in
pkgs.stdenv.mkDerivation {
  pname = "wrapped-uv";
  src = ./.;
  version = pkgs.uv.version;

  nativeBuildInputs = [
    pkgs.makeWrapper
  ];

  installPhase = ''
    mkdir -p $out/bin

    makeWrapper ${actualUv}/bin/${actualUv.meta.mainProgram} $out/bin/uv
    makeWrapper ${actualUv}/bin/${actualUv.meta.mainProgram} $out/bin/uvx --add-flags "tool uvx"
  '';
}
