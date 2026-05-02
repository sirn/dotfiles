{
  lib,
  stdenvNoCC,
  python3,
  python3Packages,
  playwright-driver,
  makeWrapper,
}:

let
  pythonEnv = python3.withPackages (ps: [ ps.playwright ]);
in
stdenvNoCC.mkDerivation {
  pname = "web-cli";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall

    install -Dm644 web.py $out/libexec/web-cli/web.py

    makeWrapper ${pythonEnv}/bin/python3 $out/bin/web \
      --add-flags "$out/libexec/web-cli/web.py" \
      --set PLAYWRIGHT_BROWSERS_PATH "${playwright-driver.browsers-chromium}" \
      --set PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS "true"

    mkdir -p $out/skills
    cp -r skills/. $out/skills/

    runHook postInstall
  '';

  meta = {
    description = "Headless Chromium CLI for agents (fetch, screenshot, eval, network, console)";
    license = lib.licenses.mit;
    mainProgram = "web";
    platforms = lib.platforms.linux ++ lib.platforms.darwin;
  };
}
