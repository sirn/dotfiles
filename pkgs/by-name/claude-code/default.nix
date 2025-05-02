{ lib, pkgs }:

pkgs.buildNpmPackage rec {
  pname = "claude-code";
  # TODO: Go back to unstable channel once unstable is upgraded beyond 0.2.99
  version = "0.2.99";

  src = pkgs.fetchzip {
    url = "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-${version}.tgz";
    hash = "sha256-MfzhISp36niovUMPU8vC+a3PTp53PpOHeGacH6nj/PY=";
  };

  npmDepsHash = "sha256-ltvGtrENPw3vWSRmVxjZ/PKPcLwNVEc1cQ02X48FeZg=";

  postPatch = ''
    cp ${./package-lock.json} package-lock.json
  '';

  dontNpmBuild = true;

  AUTHORIZED = "1";

  # `claude-code` tries to auto-update by default, this disables that functionality.
  # https://docs.anthropic.com/en/docs/agents-and-tools/claude-code/overview#environment-variables
  postInstall = ''
    wrapProgram $out/bin/claude \
      --set DISABLE_AUTOUPDATER 1
  '';

  passthru.updateScript = ./update.sh;

  meta = {
    description = "An agentic coding tool that lives in your terminal, understands your codebase, and helps you code faster";
    homepage = "https://github.com/anthropics/claude-code";
    downloadPage = "https://www.npmjs.com/package/@anthropic-ai/claude-code";
    license = lib.licenses.unfree;
    maintainers = [ lib.maintainers.malo ];
    mainProgram = "claude";
  };
}
