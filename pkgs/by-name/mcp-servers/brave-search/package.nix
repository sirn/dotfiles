{ lib, buildNpmPackage, fetchzip, jq, moreutils }:

buildNpmPackage rec {
  pname = "brave-search-mcp-server";
  version = "2.0.68";

  src = fetchzip {
    url = "https://registry.npmjs.org/@brave/${pname}/-/${pname}-${version}.tgz";
    hash = "sha256-fAoDD/G/P+BDShqLdx6aqBhgT9obVFAAzKX1F+AWsvc=";
  };

  npmDepsHash = "sha256-DEoeNpRazJIC/nddpI07YZ1NmmRMIRs0G3lSSoVaSVw=";

  postPatch = ''
    ${lib.getExe jq} 'del(.devDependencies)' package.json | ${moreutils}/bin/sponge package.json
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
