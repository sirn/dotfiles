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
  pname = "pi-hashline-edit";
  inherit version;

  src = fetchFromGitHub {
    owner = "RimuruW";
    repo = "pi-hashline-edit";
    rev = "refs/tags/v${finalAttrs.version}";
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
    cp -r index.ts package.json src prompts node_modules $out/

    runHook postInstall
  '';

  passthru.updateScript = ./update.sh;

  meta = with lib; {
    description = "Hashline read/edit tool override for pi-coding-agent";
    homepage = "https://github.com/RimuruW/pi-hashline-edit";
    license = licenses.mit;
  };
})
