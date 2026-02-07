{ lib, buildNpmPackage, fetchzip, jq, moreutils }:

buildNpmPackage rec {
  pname = "mcp-remote";
  version = "0.1.38";

  src = fetchzip {
    url = "https://registry.npmjs.org/${pname}/-/${pname}-${version}.tgz";
    hash = "sha256-axR1CvQSk69/hl2F3XT1rwvLQCysAPt+EaYU/ftN6Ho=";
  };

  npmDepsHash = "sha256-FhS6GynN+edPlcx45qTOllDSZBhelzEll63CY1a/19c=";

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
