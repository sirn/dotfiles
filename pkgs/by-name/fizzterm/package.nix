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
  version = "0.1.0";

  src = fetchgit {
    url = "https://git.sr.ht/~sirn/fizzterm";
    rev = "refs/tags/v" + version;
    hash = "sha256-3XakDwdMFZnkmRXdoh/cHFlH9cEJooF+vwYCJonckmI=";
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

  cargoHash = "sha256-/CaogP3QOQs2pO9oleR01Dq7pp1rb5AarDC3BvXWRxM=";

  passthru.updateScript = ./update.sh;

  meta = with lib; {
    description = "A persistent browser terminal with a Rust server and xterm.js";
    homepage = "https://git.sr.ht/~sirn/fizzterm";
    license = licenses.mit;
    mainProgram = "fizzterm";
  };
}
