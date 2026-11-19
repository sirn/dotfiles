{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
}:
let
  sources = lib.importJSON ./sources.json;
in

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "skill-simple-english";
  inherit (sources) version;

  src = fetchFromGitHub {
    owner = "AminBlg";
    repo = "SimpleEnglish";
    inherit (sources) rev;
    hash = sources.srcHash;
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out
    cp -r skills $out/

    # Extract the fenced rule block from prompts/system-prompt.md (same payload
    # as the upstream SessionStart hook) after stripping optional frontmatter.
    awk '
      NR == 1 && /^---[[:space:]]*$/ { in_fm = 1; next }
      in_fm && /^---[[:space:]]*$/ { in_fm = 0; next }
      in_fm { next }
      /^---[[:space:]]*$/ { fence++; next }
      fence == 1 { print }
    ' prompts/system-prompt.md > $out/prompt.md
    test -s $out/prompt.md

    runHook postInstall
  '';
  passthru.updateScript = ./update.sh;

  meta = {
    description = "Plain English writing agent skill inspired by ASD-STE100 Simplified Technical English";
    homepage = "https://github.com/AminBlg/SimpleEnglish";
    license = lib.licenses.mit;
  };
})
