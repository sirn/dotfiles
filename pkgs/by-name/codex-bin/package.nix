{
  lib,
  stdenv,
  fetchurl,
}:

let
  sources = lib.importJSON ./sources.json;
  inherit (sources) version systems;

  system =
    systems.${stdenv.hostPlatform.system}
      or (throw "Unsupported system: ${stdenv.hostPlatform.system}");
in
stdenv.mkDerivation {
  pname = "codex-bin";
  inherit version;

  src = fetchurl { inherit (system) url hash; };

  sourceRoot = ".";

  dontBuild = true;
  dontStrip = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp codex-* $out/bin/codex
    chmod +x $out/bin/codex
    runHook postInstall
  '';

  passthru.updateScript = ./update.sh;

  meta = {
    description = "OpenAI Codex CLI - lightweight coding agent for the terminal";
    homepage = "https://github.com/openai/codex";
    license = lib.licenses.asl20;
    mainProgram = "codex";
    platforms = lib.attrNames systems;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
