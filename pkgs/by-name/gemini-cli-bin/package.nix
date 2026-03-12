{
  lib,
  stdenv,
  fetchurl,
  bun,
  makeWrapper,
}:

let
  sources = lib.importJSON ./sources.json;
  inherit (sources) version src files;

  sandboxProfiles = lib.optionals stdenv.isDarwin (
    map (file: fetchurl { inherit (file) url hash; }) files
  );
in
stdenv.mkDerivation {
  pname = "gemini-cli-bin";
  inherit version;

  src = fetchurl { inherit (src) url hash; };

  dontUnpack = true;
  dontBuild = true;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin $out/share/gemini-cli
    cp $src $out/share/gemini-cli/gemini.js
    ${lib.concatMapStrings (profile: ''
      cp ${profile} $out/share/gemini-cli/$(basename ${profile})
    '') sandboxProfiles}
    makeWrapper ${lib.getExe bun} $out/bin/gemini \
      --add-flags "$out/share/gemini-cli/gemini.js"
    runHook postInstall
  '';

  passthru.updateScript = ./update.sh;

  meta = {
    description = "Google Gemini CLI - an open-source AI agent";
    homepage = "https://github.com/google-gemini/gemini-cli";
    license = lib.licenses.asl20;
    mainProgram = "gemini";
    platforms = lib.platforms.unix;
    sourceProvenance = [ lib.sourceTypes.fromSource ];
  };
}
