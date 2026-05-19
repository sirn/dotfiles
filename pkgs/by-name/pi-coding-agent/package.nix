{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  ripgrep,
}:

let
  sources = lib.importJSON ./sources.json;
  inherit (sources) version;
in
buildNpmPackage (finalAttrs: {
  pname = "pi-coding-agent";
  inherit version;

  src = fetchFromGitHub {
    owner = "earendil-works";
    repo = "pi";
    rev = "refs/tags/v${finalAttrs.version}";
    hash = sources.srcHash;
  };

  npmDepsHash = sources.npmDepsHash;

  npmWorkspace = "packages/coding-agent";
  npmRebuildFlags = [ "--ignore-scripts" ];

  buildPhase = ''
    runHook preBuild

    npx tsgo -p packages/ai/tsconfig.build.json
    npx tsgo -p packages/tui/tsconfig.build.json
    npx tsgo -p packages/agent/tsconfig.build.json
    npm run build --workspace=packages/coding-agent

    runHook postBuild
  '';

  postInstall = ''
    local nm="$out/lib/node_modules/pi-monorepo/node_modules"

    for ws in @earendil-works/pi-ai:packages/ai \
              @earendil-works/pi-agent-core:packages/agent \
              @earendil-works/pi-tui:packages/tui; do
      IFS=: read -r pkg src <<< "$ws"
      rm "$nm/$pkg"
      cp -r "$src" "$nm/$pkg"
    done

    find "$nm" -type l -lname '*/packages/*' -delete
    find "$nm/.bin" -xtype l -delete
  '';

  postFixup = ''
    wrapProgram $out/bin/pi \
      --prefix PATH : ${lib.makeBinPath [ ripgrep ]}
  '';

  passthru.updateScript = ./update.sh;

  meta = with lib; {
    description = "Pi coding agent CLI";
    homepage = "https://github.com/earendil-works/pi";
    license = licenses.mit;
    mainProgram = "pi";
    platforms = platforms.linux ++ platforms.darwin;
  };
})
