{ lib, pkgs }:

pkgs.buildNpmPackage rec {
  pname = "context7-mcp";
  version = "1.0.26";

  src = pkgs.fetchzip {
    url = "https://registry.npmjs.org/@upstash/${pname}/-/${pname}-${version}.tgz";
    hash = "sha256-KDkbOw4VksVM4QtpYBpDBG6q3qZRLRHzavf7IBebfEg=";
  };

  npmDepsHash = "sha256-598LFJUYIp9TMwak9nkgEmG9m/+Z/THP1pqsCTxb6h8=";

  postPatch = ''
    ${lib.getExe pkgs.jq} 'del(.devDependencies)' package.json | ${pkgs.moreutils}/bin/sponge package.json
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
