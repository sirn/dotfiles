{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
}:
let
  sources = lib.importJSON ./sources.json;
in

stdenvNoCC.mkDerivation {
  pname = "skill-superpowers";
  inherit (sources) version;

  src = fetchFromGitHub {
    owner = "obra";
    repo = "superpowers";
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
    description = "Disciplined software engineering workflow agent skills";
    homepage = "https://github.com/obra/superpowers";
    license = lib.licenses.mit;
  };
}
