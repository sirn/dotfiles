{ lib, pkgs }:

pkgs.buildNpmPackage rec {
  pname = "brave-search-mcp-server";
  version = "1.3.7";

  src = pkgs.fetchzip {
    url = "https://registry.npmjs.org/@brave/${pname}/-/${pname}-${version}.tgz";
    hash = "sha256-XbR4xTPcEQzYmSu7OYfIl365obfjpDopHjSX/1yAygQ=";
  };

  npmDepsHash = "sha256-Oz0NQneaOERKItchKW+GUpP06q2+elbfIcATOohCyr0=";

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
