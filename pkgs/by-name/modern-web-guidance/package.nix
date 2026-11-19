{
  lib,
  stdenvNoCC,
  fetchurl,
  makeWrapper,
  nodejs,
}:
let
  sources = lib.importJSON ./sources.json;
in

stdenvNoCC.mkDerivation {
  pname = "modern-web-guidance";
  inherit (sources) version;

  src = fetchurl {
    inherit (sources) url;
    hash = sources.srcHash;
  };

  nativeBuildInputs = [ makeWrapper ];

  # Invoke the locally installed CLI binary instead of fetching via npx/pnpx at runtime.
  postPatch = ''
    substituteInPlace skills/modern-web-guidance/SKILL.md \
      --replace-fail "npx -y modern-web-guidance@latest" "modern-web-guidance" \
      --replace-fail "pnpx modern-web-guidance@latest" "modern-web-guidance"
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp -r skills $out/
    chmod +x $out/skills/modern-web-guidance/modern-web.mjs

    makeWrapper ${lib.getExe nodejs} $out/bin/modern-web-guidance \
      --add-flags "$out/skills/modern-web-guidance/modern-web.mjs" \
      --set-default DISABLE_TELEMETRY 1

    runHook postInstall
  '';
  passthru.updateScript = ./update.sh;

  meta = {
    description = "Web platform guidance CLI and agent skills from Chrome, Edge, Firefox, and MDN";
    homepage = "https://github.com/GoogleChrome/modern-web-guidance";
    license = [
      lib.licenses.asl20
      lib.licenses.cc-by-40
    ];
    mainProgram = "modern-web-guidance";
  };
}
