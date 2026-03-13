{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
}:

let
  sources = lib.importJSON ./sources.json;
  inherit (sources) version systems;

  system =
    systems.${stdenv.hostPlatform.system}
      or (throw "Unsupported system: ${stdenv.hostPlatform.system}");
in
stdenv.mkDerivation {
  pname = "rtk";
  inherit version;

  src = fetchurl { inherit (system) url hash; };

  nativeBuildInputs = lib.optionals stdenv.hostPlatform.isLinux [ autoPatchelfHook ];

  sourceRoot = ".";

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp rtk $out/bin/rtk
    chmod +x $out/bin/rtk
    runHook postInstall
  '';

  passthru.updateScript = ./update.sh;

  meta = {
    description = "RTK CLI - AI-powered CLI for development workflows";
    homepage = "https://github.com/rtk-ai/rtk";
    license = lib.licenses.mit;
    mainProgram = "rtk";
    platforms = lib.attrNames systems;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
