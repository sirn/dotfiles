{ lib, buildFHSEnv, stdenv, uv, makeWrapper }:

let
  fhsUv = buildFHSEnv {
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
    if stdenv.isLinux
    then fhsUv
    else uv;
in
stdenv.mkDerivation {
  pname = "wrapped-uv";
  src = ./.;
  version = uv.version;

  nativeBuildInputs = [
    makeWrapper
  ];

  installPhase = ''
    mkdir -p $out/bin

    makeWrapper ${actualUv}/bin/${actualUv.meta.mainProgram} $out/bin/uv --add-flags "--managed-python"
    makeWrapper ${actualUv}/bin/${actualUv.meta.mainProgram} $out/bin/uvx --add-flags "--managed-python" --add-flags "tool uvx"
  '';
}
