{
  fetchzip,
  lib,
  stdenv,
}:

let
  appName = "OmniWM.app";
  version = "0.4.8";
in
stdenv.mkDerivation {
  pname = "omniwm";

  inherit version;

  src = fetchzip {
    url = "https://github.com/BarutSRB/OmniWM/releases/download/v${version}/OmniWM-v${version}.zip";
    hash = "sha256-XowPlMffST0mId++Gz4ukFhGLqQiKaj387/U1spDgSo=";
  };

  installPhase = ''
    runHook preInstall
    mkdir -p $out/Applications/${appName}/Contents
    # fetchzip strips the top-level OmniWM.app directory,
    # so the extracted source has Contents/ at the root.
    # Strip macOS resource fork files (._*) that fetchzip preserves.
    find Contents -name '._*' -delete
    mv Contents/* $out/Applications/${appName}/Contents/
    mkdir -p $out/bin
    ln -s $out/Applications/${appName}/Contents/MacOS/omniwmctl $out/bin/omniwmctl
    runHook postInstall
  '';

  meta = {
    license = lib.licenses.gpl2Only;
    mainProgram = "omniwmctl";
    homepage = "https://github.com/BarutSRB/OmniWM";
    description = "Niri and Hyprland inspired tiling window manager for macOS";
    platforms = lib.platforms.darwin;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
