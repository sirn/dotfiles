{
  lib,
  stdenv,
  fetchurl,
  nodejs_24,
}:

let
  sources = lib.importJSON ./sources.json;
  inherit (sources) version;
  nodejs = nodejs_24;
in
stdenv.mkDerivation rec {
  pname = "portless";
  inherit version;

  src = fetchurl {
    url = "https://registry.npmjs.org/portless/-/portless-${version}.tgz";
    hash = sources.srcHash;
  };

  # The published tarball ships a prebuilt, dependency-free `dist` (only
  # Node built-ins are imported), so there is nothing to build.
  dontBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib/portless $out/bin
    cp -r dist/. $out/lib/portless/

    # Wrapper so the relative `./chunk-*.js` imports inside dist resolve
    # correctly; cli.js stays in lib/ and the bin just execs it with the
    # bundled nodejs.
    cat >$out/bin/portless <<EOF
    #!${stdenv.shell}
    exec ${lib.getExe nodejs} $out/lib/portless/cli.js "\$@"
    EOF
    chmod +x $out/bin/portless

    runHook postInstall
  '';

  passthru.updateScript = ./update.sh;

  meta = {
    description = "Replace port numbers with stable, named .localhost URLs for local development";
    homepage = "https://portless.sh";
    license = lib.licenses.asl20;
    mainProgram = "portless";
    platforms = lib.platforms.linux ++ lib.platforms.darwin;
  };
}
