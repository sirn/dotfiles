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
  pname = "synthetic-search-cli";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    runHook preInstall

    install -Dm644 synthetic-search.py $out/libexec/synthetic-search-cli/synthetic-search.py

    makeWrapper ${pythonEnv}/bin/python3 $out/bin/synthetic-search \
      --add-flags "$out/libexec/synthetic-search-cli/synthetic-search.py"

    mkdir -p $out/skills
    cp -r skills/. $out/skills/

    runHook postInstall
  '';

  meta = {
    description = "Web search CLI for agents via the Synthetic Search API";
    license = lib.licenses.mit;
    mainProgram = "synthetic-search";
    platforms = lib.platforms.linux ++ lib.platforms.darwin;
  };
}
