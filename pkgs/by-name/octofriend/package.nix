{ lib, pkgs }:

pkgs.buildNpmPackage rec {
  pname = "octofriend";
  version = "0.0.48";

  src = pkgs.fetchzip {
    url = "https://registry.npmjs.org/${pname}/-/${pname}-${version}.tgz";
    hash = "sha256-me/EpDWBmt5UNv5sPLTJzUEXXqIShiuDy16K09CuqHc=";
  };

  npmDepsHash = "sha256-C0TDkSoGkf/ka6nQfM2UkQdo5viZQNtw6cCBbOIBvxk=";

  dontNpmBuild = true;

  postPatch = ''
    ${lib.getExe pkgs.jq} 'del(.devDependencies)' package.json | ${pkgs.moreutils}/bin/sponge package.json
    sed -i 's|/bin/bash|bash|' dist/source/transports/local.js
    cp ${./package-lock.json} package-lock.json
  '';

  passthru.updateScript = ./update.sh;

  meta = {
    description = "An open-source coding helper. Very friendly!";
    homepage = "https://github.com/synthetic-lab/octofriend";
    downloadPage = "https://www.npmjs.com/package/octofriend";
    license = lib.licenses.mit;
    maintainers = [ ];
    mainProgram = "octofriend";
  };
}
