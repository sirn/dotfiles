{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
}:
let
  sources = lib.importJSON ./sources.json;
in
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "mitsuhiko-agent-stuff";
  inherit (sources) version;

  src = fetchFromGitHub {
    owner = "mitsuhiko";
    repo = "agent-stuff";
    inherit (sources) rev;
    hash = sources.srcHash;
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out
    cp -r . $out/

    runHook postInstall
  '';
  passthru.updateScript = ./update.sh;

  meta = {
    description = "Mitsuhiko's agent helper commands and skills";
    homepage = "https://github.com/mitsuhiko/agent-stuff";
    license = lib.licenses.asl20;
  };
})
