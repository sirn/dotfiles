{
  lib,
  stdenv,
  fetchFromGitHub,
}:

let
  sources = lib.importJSON ./sources.json;
  inherit (sources) version;
in

stdenv.mkDerivation (finalAttrs: {
  pname = "pi-tool-repair";
  inherit version;

  src = fetchFromGitHub {
    owner = "monotykamary";
    repo = "pi-tool-repair";
    rev = sources.rev;
    hash = sources.srcHash;
  };

  dontBuild = true;
  dontPatchELF = true;
  dontStrip = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out
    cp -r tool-repair.ts package.json src README.md $out/

    runHook postInstall
  '';

  passthru.updateScript = ./update.sh;

  meta = with lib; {
    description = "Validate-then-repair extension for pi — fixes common LLM tool-call mistakes before tools execute";
    homepage = "https://github.com/monotykamary/pi-tool-repair";
    license = licenses.mit;
    platforms = platforms.linux ++ platforms.darwin;
  };
})
