{
  lib,
  buildNpmPackage,
  fetchurl,
  nodejs,
  qemu,
  e2fsprogs,
  lz4,
  cpio,
  makeWrapper,
}:

let
  sources = lib.importJSON ./sources.json;
  inherit (sources) version;
in
buildNpmPackage rec {
  pname = "gondolin";
  inherit version;

  src = fetchurl {
    url = "https://registry.npmjs.org/@earendil-works/gondolin/-/gondolin-${version}.tgz";
    hash = sources.srcHash;
  };

  # The published tarball ships prebuilt `dist`; only dependencies need
  # installing.  The tarball omits the lockfile, so vendor it.
  dontNpmBuild = true;

  postPatch = ''
    cp ${./package-lock.json} ./package-lock.json
  '';

  inherit nodejs;

  npmDepsHash = sources.npmDepsHash;

  # Gondolin needs QEMU on PATH at runtime, and e2fsprogs / lz4 / cpio
  # for `gondolin build` (custom guest image creation).
  nativeBuildInputs = [ makeWrapper ];

  postInstall = ''
    wrapProgram $out/bin/gondolin \
      --prefix PATH : ${
        lib.makeBinPath [
          qemu
          e2fsprogs
          lz4
          cpio
        ]
      }
  '';

  passthru.updateScript = ./update.sh;

  meta = with lib; {
    description = "Lightweight micro-VM sandboxes for AI agent workloads";
    homepage = "https://earendil-works.github.io/gondolin/";
    license = licenses.mit;
    mainProgram = "gondolin";
    platforms = platforms.unix;
  };
}
