{
  lib,
  stdenvNoCC,
  python3,
  makeWrapper,
}:

stdenvNoCC.mkDerivation {
  pname = "exa-cli";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall

    install -Dm644 exa.py $out/libexec/exa-cli/exa.py

    makeWrapper ${python3}/bin/python3 $out/bin/exa \
      --add-flags "$out/libexec/exa-cli/exa.py"

    mkdir -p $out/skills
    cp -r skills/. $out/skills/

    runHook postInstall
  '';

  meta = {
    description = "CLI for the Exa AI API — search, contents, websets, and code context";
    license = lib.licenses.mit;
    mainProgram = "exa";
    platforms = lib.platforms.linux ++ lib.platforms.darwin;
  };
}
