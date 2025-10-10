{ lib, pkgs }:

pkgs.buildNpmPackage rec {
  pname = "context7-mcp";
  version = "1.0.21";

  src = pkgs.fetchzip {
    url = "https://registry.npmjs.org/@upstash/${pname}/-/${pname}-${version}.tgz";
    hash = "sha256-ousyG04vRrpTo0K3YNViLyYN9NFKkSYpZIg4V1UEgE8=";
  };

  npmDepsHash = "sha256-Xqc27EF0ti45whfUMBEOYZzSFQfKrtD/TAxlvZ2sSp8=";

  postPatch = ''
    ${lib.getExe pkgs.jq} 'del(.devDependencies)' package.json | ${pkgs.moreutils}/bin/sponge package.json
    cp ${./package-lock.json} package-lock.json
  '';

  dontNpmBuild = true;

  passthru.updateScript = pkgs.writeScript "update-context7" ''
    #!/usr/bin/env nix-shell
    #!nix-shell -i bash --packages nodejs nix-update git
    set -euo pipefail
    version=$(npm view @upstash/context7-mcp version)
    nix-update local.mcpServers.context7 --version="$version" --generate-lockfile
  '';

  meta = {
    description = "Up-to-date code documentation for LLMs and AI code editors";
    homepage = "https://github.com/upstash/context7";
    downloadPage = "https://www.npmjs.com/package/@upstash/context7-mcp";
    license = lib.licenses.mit;
    maintainers = [ ];
    mainProgram = "context7-mcp";
  };
}
