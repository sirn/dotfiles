{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  unzip,
}:

let
  sources = lib.importJSON ./sources.json;
  inherit (sources) version systems;

  system =
    systems.${stdenv.hostPlatform.system}
      or (throw "Unsupported system: ${stdenv.hostPlatform.system}");
in
stdenv.mkDerivation {
  pname = "opencode-bin";
  inherit version;

  src = fetchurl { inherit (system) url hash; };

  nativeBuildInputs =
    lib.optionals stdenv.hostPlatform.isLinux [ autoPatchelfHook ]
    ++ lib.optionals stdenv.hostPlatform.isDarwin [ unzip ];

  buildInputs = lib.optionals stdenv.hostPlatform.isLinux [ stdenv.cc.cc.lib ];

  sourceRoot = ".";

  dontBuild = true;
  dontStrip = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp opencode $out/bin/opencode
    chmod +x $out/bin/opencode
    runHook postInstall
  '';

  passthru.updateScript = ./update.sh;

  meta = {
    description = "OpenCode - open-source AI coding agent for the terminal";
    homepage = "https://github.com/anomalyco/opencode";
    license = lib.licenses.mit;
    mainProgram = "opencode";
    platforms = lib.attrNames systems;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
  };
}
