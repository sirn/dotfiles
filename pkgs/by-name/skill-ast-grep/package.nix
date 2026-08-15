{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
}:
let
  sources = lib.importJSON ./sources.json;
in

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "skill-ast-grep";
  inherit (sources) version;

  src = fetchFromGitHub {
    owner = "ast-grep";
    repo = "agent-skill";
    inherit (sources) rev;
    hash = sources.srcHash;
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out
    cp -r ast-grep/.claude-plugin ast-grep/skills $out/

    runHook postInstall
  '';
  passthru.updateScript = ./update.sh;

  meta = {
    description = "ast-grep agent skill for structural code search";
    homepage = "https://github.com/ast-grep/agent-skill";
    license = lib.licenses.mit;
  };
})
