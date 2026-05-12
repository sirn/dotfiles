{
  lib,
  stdenvNoCC,
  python3,
  makeWrapper,
}:

let
  pythonEnv = python3.withPackages (ps: [ ]);
in
stdenvNoCC.mkDerivation {
  pname = "context7-cli";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall

    install -Dm644 context7.py $out/libexec/context7-cli/context7.py

    makeWrapper ${pythonEnv}/bin/python3 $out/bin/context7 \
      --add-flags "$out/libexec/context7-cli/context7.py"

    mkdir -p $out/skills
    cp -r skills/. $out/skills/

    runHook postInstall
  '';

  meta = {
    description = "Retrieve documentation context for libraries using the Context7 API";
    license = lib.licenses.mit;
    mainProgram = "context7";
    platforms = lib.platforms.linux ++ lib.platforms.darwin;
  };
}
