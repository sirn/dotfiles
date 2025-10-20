{ lib, pkgs }:

pkgs.buildNpmPackage rec {
  pname = "octofriend";
  version = "0.0.45";

  src = pkgs.fetchzip {
    url = "https://registry.npmjs.org/${pname}/-/${pname}-${version}.tgz";
    hash = "sha256-HBayIYeAmT/dNhG4Z6XUeh0Ws54kie62Bmbf1foM7Jk=";
  };

  npmDepsHash = "sha256-ksUGU98IVRRlslJIioz2qAZ9HWCBdk/43ZLaYPrs0WI=";

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
