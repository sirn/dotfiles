{
  lib,
  stdenv,
  fetchFromGitHub,
  fetchNpmDeps,
  nodejs,
  npmHooks,
}:

let
  sources = lib.importJSON ./sources.json;
  inherit (sources) version;

  stripPeerAndDevDeps = ''
    ${lib.getExe nodejs} -e "
      const pj = JSON.parse(require('fs').readFileSync('package.json','utf8'));
      delete pj.peerDependencies;
      delete pj.devDependencies;
      require('fs').writeFileSync('package.json', JSON.stringify(pj, null, 2) + '\n');
    "
  '';
in
stdenv.mkDerivation (finalAttrs: {
  pname = "pi-vcc";
  inherit version;

  src = fetchFromGitHub {
    owner = "monotykamary";
    repo = "pi-vcc";
    rev = "1994b2611e9ae8aa7afe6a670c1122578b198477";
    hash = sources.srcHash;
  };

  npmDeps = fetchNpmDeps {
    inherit (finalAttrs) src;
    hash = sources.npmDepsHash;
    postPatch = ''
      cp ${./package-lock.json} package-lock.json
      ${stripPeerAndDevDeps}
    '';
  };

  nativeBuildInputs = [
    nodejs
    npmHooks.npmConfigHook
  ];

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
    ${stripPeerAndDevDeps}
  '';

  dontBuild = true;
  dontPatchELF = true;
  dontStrip = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out
    cp -r index.ts package.json src $out/
    cp -r ${finalAttrs.npmDeps} $out/node_modules

    runHook postInstall
  '';

  passthru.updateScript = ./update.sh;

  meta = with lib; {
    description = "Algorithmic conversation compactor for pi - transcript-preserving structured summaries, no LLM calls";
    homepage = "https://github.com/monotykamary/pi-vcc";
    license = licenses.mit;
    platforms = platforms.linux ++ platforms.darwin;
  };
})
