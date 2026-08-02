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

  # The published CLI never passes `publicUrl` to `createServer`, and after
  # creation it calls `server.setPublicUrl(resolved.url)` which overwrites any
  # value we inject.
  patches = [ ./public-url.patch ];

  postPatch = ''
    cp ${./package-lock.json} ./package-lock.json
  '';

  # The server rejects WebSocket upgrades from non-RFC-1918 source IPs when
  # bound to a non-loopback address. Access is already restricted by the NixOS
  # firewall, so disable the redundant check to allow e.g. TEST-NET-1 tunnels.
  postConfigure = ''
    sed -i 's/!isAllowedSourceIp(remoteAddress, host)/false/' \
      node_modules/@monotykamary/localterm-server/dist/index.js
  '';

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
