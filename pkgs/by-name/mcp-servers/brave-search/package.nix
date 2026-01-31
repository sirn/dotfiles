{ lib, buildNpmPackage, fetchzip, jq, moreutils }:

buildNpmPackage rec {
  pname = "brave-search-mcp-server";
  version = "2.0.72";

  src = fetchzip {
    url = "https://registry.npmjs.org/@brave/${pname}/-/${pname}-${version}.tgz";
    hash = "sha256-S5GAWoVSbLignISljGOVMnSbEMf5EMP8i0p+qolH+bI=";
  };

  npmDepsHash = "sha256-hh03nFQnTwOdJ4qNY8/l26Xqh74skry5yCTEGnHf4bI=";

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
