{
  lib,
  buildNpmPackage,
  fetchurl,
  nodejs_22,
}:

let
  sources = lib.importJSON ./sources.json;
  inherit (sources) version;
in
buildNpmPackage rec {
  pname = "localterm";
  inherit version;

  src = fetchurl {
    url = "https://registry.npmjs.org/@monotykamary/localterm/-/localterm-${version}.tgz";
    hash = sources.srcHash;
  };

  # The published tarball ships prebuilt `dist` and `terminal` assets; only
  # dependencies (including the native node-pty module) need installing.
  dontNpmBuild = true;

  # The npm tarball omits the lockfile; vendor it so npmDepsHash is stable.
  postPatch = ''
    cp ${./package-lock.json} ./package-lock.json
  '';

  # node-pty is built from source via node-gyp, which needs Python.
  nodejs = nodejs_22;

  # node-pty ships prebuilt binaries whose `spawn-helper` lacks the executable
  # bit, which is immutable in the Nix store and breaks PTY spawning. Forcing a
  # from-source rebuild (node-pty's own prebuild.js honors this) produces a
  # correctly permissioned spawn-helper.
  env.npm_config_build_from_source = "true";

  npmDepsHash = sources.npmDepsHash;

  passthru.updateScript = ./update.sh;

  meta = {
    description = "Browser-based terminal: one browser tab is one PTY session";
    homepage = "https://github.com/monotykamary/localterm";
    license = lib.licenses.mit;
    mainProgram = "localterm";
    platforms = lib.platforms.linux ++ lib.platforms.darwin;
  };
}
