{ lib, pkgs }:

pkgs.buildNpmPackage rec {
  pname = "octofriend";
  version = "0.0.37";

  src = pkgs.fetchzip {
    url = "https://registry.npmjs.org/${pname}/-/${pname}-${version}.tgz";
    hash = "sha256-glI1OJS9VgDyDx1ZVLswNb4elhV7ahwOZKNWBBZvUWA=";
  };

  npmDepsHash = "sha256-XnYA/CBNW0KJoGpAEH/s48YZzSQv8yKcBqUCbI8Xiwc=";

  dontNpmBuild = true;

  postPatch = ''
    ${lib.getExe pkgs.jq} 'del(.devDependencies)' package.json | ${pkgs.moreutils}/bin/sponge package.json
    sed -i 's|/bin/bash|bash|' dist/source/transports/local.js
    cp ${./package-lock.json} package-lock.json
  '';

  meta = {
    description = "An open-source coding helper. Very friendly!";
    homepage = "https://github.com/synthetic-lab/octofriend";
    downloadPage = "https://www.npmjs.com/package/octofriend";
    license = lib.licenses.mit;
    maintainers = [ ];
    mainProgram = "octofriend";
  };
}
