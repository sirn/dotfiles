{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  makeWrapper,
  bubblewrap ? null,
  socat ? null,
  procps ? null,
}:

let
  sources = lib.importJSON ./sources.json;
  inherit (sources) version systems;

  system =
    systems.${stdenv.hostPlatform.system}
      or (throw "Unsupported system: ${stdenv.hostPlatform.system}");
in
stdenv.mkDerivation {
  pname = "claude-code-bin";
  inherit version;

  src = fetchurl { inherit (system) url hash; };

  nativeBuildInputs = [
    makeWrapper
  ]
  ++ lib.optionals stdenv.hostPlatform.isLinux [ autoPatchelfHook ];
  buildInputs = lib.optionals stdenv.hostPlatform.isLinux [ stdenv.cc.cc.lib ];

  dontUnpack = true;
  dontBuild = true;
  dontStrip = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp $src $out/bin/.claude-wrapped
    chmod +x $out/bin/.claude-wrapped
    makeWrapper $out/bin/.claude-wrapped $out/bin/claude \
      --prefix PATH : "${
        lib.makeBinPath (
          lib.optionals stdenv.hostPlatform.isLinux [
            bubblewrap
            socat
            procps
          ]
        )
      }"
    runHook postInstall
  '';

  passthru.updateScript = ./update.sh;

  meta = {
    description = "Claude Code - an agentic coding tool that lives in your terminal";
    homepage = "https://github.com/anthropics/claude-code";
    license = lib.licenses.unfree;
    mainProgram = "claude";
    platforms = lib.attrNames systems;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
