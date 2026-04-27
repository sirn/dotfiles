{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
}:
let
  sources = lib.importJSON ./sources.json;
in
stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "skill-brave-search";
  inherit (sources) version;

  src = fetchFromGitHub {
    owner = "brave";
    repo = "brave-search-skills";
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
    description = "Official Brave Search API skills for AI coding agents";
    homepage = "https://github.com/brave/brave-search-skills";
    license = lib.licenses.mit;
  };
})
