{
  lib,
  stdenv,
  rustPlatform,
  fetchgit,
  fetchNpmDeps,
  npmHooks,
  nodejs,
  trunk,
  wasm-bindgen-cli,
  binaryen,
  lld,
}:

rustPlatform.buildRustPackage rec {
  pname = "fizzterm";
  version = "0.2.1";

  src = fetchgit {
    url = "https://git.sr.ht/~sirn/fizzterm";
    rev = "refs/tags/v" + version;
    hash = "sha256-f9anOvvmbPHAVCd6IfP/1Vi21hQ7PrPkI0onqoCnd0c=";
  };

  npmDeps = fetchNpmDeps {
    inherit src;
    name = "fizzterm-ui-npm-deps";
    hash = "sha256-8+P2EDn7MaFLc5EASeI9tc2PoP+vADIYTHWzjEPgcoA=";
    postPatch = "cp fizzterm-ui/package-lock.json ./package-lock.json";
  };

  nativeBuildInputs = [
    nodejs
    npmHooks.npmConfigHook
    trunk
    wasm-bindgen-cli
    binaryen
    lld
  ];

  npmRoot = "fizzterm-ui";
  npmBuildScript = "build";
  dontNpmBuild = true;
  dontNpmInstall = true;

  preBuild = ''
    (cd fizzterm-ui && trunk build --release --dist target/release-dist && cp -R target/release-dist dist)
  '';

  cargoBuildFlags = [
    "-p"
    "fizzterm"
  ];
  doCheck = false;

  cargoHash = "sha256-SnC2KNj99x1XDPA+zq0qY3IyHW/m8/fQZa79cmt30H0=";

  passthru.updateScript = ./update.sh;

  meta = with lib; {
    description = "A persistent browser terminal with a Rust server and xterm.js";
    homepage = "https://git.sr.ht/~sirn/fizzterm";
    license = licenses.mit;
    mainProgram = "fizzterm";
  };
}
