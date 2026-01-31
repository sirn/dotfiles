{ lib, buildNpmPackage, fetchzip, jq, moreutils }:

buildNpmPackage rec {
  pname = "context7-mcp";
  version = "2.1.1";

  src = fetchzip {
    url = "https://registry.npmjs.org/@upstash/${pname}/-/${pname}-${version}.tgz";
    hash = "sha256-f0gYhM0bUr5pBUUdhHIE1IQ0T/zZmC6kwJP0rRLFGAg=";
  };

  npmDepsHash = "sha256-3rPRh77h/ZadnO/563J5/L90MsiOhYjtxXSMlswdoyM=";

  postPatch = ''
    ${lib.getExe jq} 'del(.devDependencies)' package.json | ${moreutils}/bin/sponge package.json
    cp ${./package-lock.json} package-lock.json
  '';

  dontNpmBuild = true;

  passthru.updateScript = ./update.sh;

  meta = {
    description = "Up-to-date code documentation for LLMs and AI code editors";
    homepage = "https://github.com/upstash/context7";
    downloadPage = "https://www.npmjs.com/package/@upstash/context7-mcp";
    license = lib.licenses.mit;
    maintainers = [ ];
    mainProgram = "context7-mcp";
  };
}
