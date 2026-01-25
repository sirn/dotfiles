{ lib, buildNpmPackage, fetchzip, jq, moreutils }:

buildNpmPackage rec {
  pname = "mcp-remote";
  version = "0.1.37";

  src = fetchzip {
    url = "https://registry.npmjs.org/${pname}/-/${pname}-${version}.tgz";
    hash = "sha256-ApX7lwEF5zE5CyDNfnttCLpcpTVrLMkNfA3msxYtH8w=";
  };

  npmDepsHash = "sha256-RqJvrIAqZYsG1uobNVRBmOaZzXVifY4OFASLjjblKSk=";

  postPatch = ''
    ${lib.getExe jq} 'del(.devDependencies)' package.json | ${moreutils}/bin/sponge package.json
    cp ${./package-lock.json} package-lock.json
  '';

  dontNpmBuild = true;

  passthru.updateScript = ./update.sh;

  meta = {
    description = "Connect to remote MCP servers over SSE/HTTP from stdio-only clients";
    homepage = "https://github.com/geelen/mcp-remote";
    downloadPage = "https://www.npmjs.com/package/mcp-remote";
    license = lib.licenses.mit;
    maintainers = [ ];
    mainProgram = "mcp-remote";
  };
}
