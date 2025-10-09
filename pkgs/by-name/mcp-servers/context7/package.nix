{ lib, pkgs }:

pkgs.buildNpmPackage rec {
  pname = "context7-mcp";
  version = "1.0.16";

  src = pkgs.fetchzip {
    url = "https://registry.npmjs.org/@upstash/${pname}/-/${pname}-${version}.tgz";
    hash = "sha256-9uo5JdH/us1HwOGz4B1db68NZYTIe6S0+Bn2R9iuQQ0=";
  };

  npmDepsHash = "sha256-UTK7l41nSKKzTW82rfIGkgzdmxPmwv1vH8Vs7peXxNA=";

  postPatch = ''
    ${lib.getExe pkgs.jq} 'del(.devDependencies)' package.json | ${pkgs.moreutils}/bin/sponge package.json
    cp ${./package-lock.json} package-lock.json
  '';

  dontNpmBuild = true;

  passthru.updateScript = pkgs.writeScript "update-claude-code" ''
    #!/usr/bin/env nix-shell
    #!nix-shell --pure -i bash --packages nodejs nix-update git
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
