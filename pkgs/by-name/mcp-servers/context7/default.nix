{ lib, pkgs }:

pkgs.buildNpmPackage rec {
  pname = "context7-mcp";
  version = "1.0.16";

  src = pkgs.fetchzip {
    url = "https://registry.npmjs.org/@upstash/${pname}/-/${pname}-${version}.tgz";
    hash = "sha256-9uo5JdH/us1HwOGz4B1db68NZYTIe6S0+Bn2R9iuQQ0=";
  };

  npmDepsHash = "sha256-wAqXOfOg5+mBMetv5LuimTCzWt6Rk+F3MEbwPYbL7Mc=";

  postPatch = ''
    ${lib.getExe pkgs.jq} 'del(.devDependencies)' package.json | ${pkgs.moreutils}/bin/sponge package.json
    cp ${./package-lock.json} package-lock.json
  '';

  dontNpmBuild = true;

  meta = {
    description = "Up-to-date code documentation for LLMs and AI code editors";
    homepage = "https://github.com/upstash/context7";
    downloadPage = "https://www.npmjs.com/package/@upstash/context7-mcp";
    license = lib.licenses.mit;
    maintainers = [ ];
    mainProgram = "context7-mcp";
  };
}
