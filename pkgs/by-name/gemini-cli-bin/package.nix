{
  lib,
  stdenv,
  fetchurl,
  bun,
  makeWrapper,
}:

let
  version = "0.33.0";
in
stdenv.mkDerivation {
  pname = "gemini-cli-bin";
  inherit version;

  src = fetchurl {
    url = "https://github.com/google-gemini/gemini-cli/releases/download/v${version}/gemini.js";
    hash = "sha256-HRNPHK5qXE8zvL2ZsHMe7e/hhbJx8jfQUknW5Z3KErU=";
  };

  dontUnpack = true;
  dontBuild = true;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin $out/share/gemini-cli
    cp $src $out/share/gemini-cli/gemini.js
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
