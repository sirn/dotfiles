{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
}:
let
  sources = lib.importJSON ./sources.json;
in

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "skill-tuicr";
  inherit (sources) version;

  src = fetchFromGitHub {
    owner = "agavra";
    repo = "tuicr";
    inherit (sources) rev;
    hash = sources.srcHash;
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out
    cp -r skills $out/

    runHook postInstall
  '';
  passthru.updateScript = ./update.sh;

  meta = {
    description = "tuicr agent skill for terminal code review sessions";
    homepage = "https://github.com/agavra/tuicr";
    license = lib.licenses.mit;
  };
})
