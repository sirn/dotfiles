{ lib, pkgs }:

pkgs.buildNpmPackage rec {
  pname = "brave-search-mcp-server";
  version = "2.0.24";

  src = pkgs.fetchzip {
    url = "https://registry.npmjs.org/@brave/${pname}/-/${pname}-${version}.tgz";
    hash = "sha256-prco9kgRiCfTpUZoCFivkWWRlUgv+htp1AVlNf4/bGc=";
  };

  npmDepsHash = "sha256-LdL2t/PF4JdXvX6ULt/68MijmlztiwDj+GO6zExRaJA=";

  postPatch = ''
    ${lib.getExe pkgs.jq} 'del(.devDependencies)' package.json | ${pkgs.moreutils}/bin/sponge package.json
    sed -i '/"prepare"/d' package.json
    cp ${./package-lock.json} package-lock.json
  '';

  dontNpmBuild = true;

  meta = {
    description = "An MCP server implementation that integrates the Brave Search API";
    homepage = "https://github.com/brave/brave-search-mcp-server";
    downloadPage = "https://www.npmjs.com/package/@brave/brave-search-mcp-server";
    license = lib.licenses.mit;
    maintainers = [ ];
    mainProgram = "brave-search-mcp-server";
  };
}
