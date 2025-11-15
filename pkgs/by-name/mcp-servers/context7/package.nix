{ lib, pkgs }:

pkgs.buildNpmPackage rec {
  pname = "context7-mcp";
  version = "1.0.27";

  src = pkgs.fetchzip {
    url = "https://registry.npmjs.org/@upstash/${pname}/-/${pname}-${version}.tgz";
    hash = "sha256-4qn6IXDsfj48P4fJCy5spkZS76ZpQ4oQwRHG3EmRJK4=";
  };

  npmDepsHash = "sha256-vsxzJ1HT8YyU6oCS2w/u8OH+foSOpXXRCaKIi5WOiPA=";

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
