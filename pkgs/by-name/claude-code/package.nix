{ lib, pkgs }:

pkgs.buildNpmPackage (finalAttrs: {
  pname = "claude-code";
  version = "2.0.5";

  src = pkgs.fetchzip {
    url = "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-${finalAttrs.version}.tgz";
    hash = "sha256-ZAolpT/NW48NpIoY2jUzbBlcHmyNcw+G1GhZ40qtJoY=";
  };

  npmDepsHash = "sha256-oI1a8QdkHRPGeSQjGDFAX2lsojbJYyuUTDXw1wohG3U=";

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';

  dontNpmBuild = true;

  env.AUTHORIZED = "1";

  # `claude-code` tries to auto-update by default, this disables that functionality.
  # https://docs.anthropic.com/en/docs/agents-and-tools/claude-code/overview#environment-variables
  # The DEV=true env var causes claude to crash with `TypeError: window.WebSocket is not a constructor`
  postInstall = ''
    wrapProgram $out/bin/claude \
      --set DISABLE_AUTOUPDATER 1 \
      --unset DEV
  '';

  passthru.updateScript = pkgs.writeScript "update-claude-code" ''
    #!/usr/bin/env nix-shell
    #!nix-shell --pure -i bash --packages nodejs nix-update git
    set -euo pipefail
    version=$(npm view @anthropic-ai/claude-code version)
    AUTHORIZED=1 NIXPKGS_ALLOW_UNFREE=1 nix-update local.claude-code --version="$version" --generate-lockfile
  '';

  meta = {
    description = "Agentic coding tool that lives in your terminal, understands your codebase, and helps you code faster";
    homepage = "https://github.com/anthropics/claude-code";
    downloadPage = "https://www.npmjs.com/package/@anthropic-ai/claude-code";
    license = lib.licenses.unfree;
    maintainers = with lib.maintainers; [
      malo
      markus1189
      omarjatoi
      xiaoxiangmoe
    ];
    mainProgram = "claude";
  };
})
