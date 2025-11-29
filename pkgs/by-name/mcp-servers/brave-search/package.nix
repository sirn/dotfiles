{ lib, pkgs }:

pkgs.buildNpmPackage rec {
  pname = "brave-search-mcp-server";
  version = "2.0.61";

  src = pkgs.fetchzip {
    url = "https://registry.npmjs.org/@brave/${pname}/-/${pname}-${version}.tgz";
    hash = "sha256-VjI02n0x4ciWCZYZDAwEl+AlrQq+dXPbpngnuz7SctA=";
  };

  npmDepsHash = "sha256-bhda7FQXBThPQwYF4UKeQDVCCUIgEsxGx7hcTC7DUc4=";

  postPatch = ''
    ${lib.getExe pkgs.jq} 'del(.devDependencies)' package.json | ${pkgs.moreutils}/bin/sponge package.json
    sed -i '/"prepare"/d' package.json
    cp ${./package-lock.json} package-lock.json
  '';

  dontNpmBuild = true;

  passthru.updateScript = ./update.sh;

  meta = {
    description = "An MCP server implementation that integrates the Brave Search API";
    homepage = "https://github.com/brave/brave-search-mcp-server";
    downloadPage = "https://www.npmjs.com/package/@brave/brave-search-mcp-server";
    license = lib.licenses.mit;
    maintainers = [ ];
    mainProgram = "brave-search-mcp-server";
  };
}
