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
  version = "0.3.1";

  src = fetchgit {
    url = "https://git.sr.ht/~sirn/fizzterm";
    rev = "refs/tags/v" + version;
    hash = "sha256-F52pCTOnIIWowvUFd6ds4Md0RgHo273mqAdIfut8B9s=";
  };

  npmDeps = fetchNpmDeps {
    inherit src;
    name = "fizzterm-ui-npm-deps";
    hash = "sha256-BVzkFG9tQXd04K/IX5+Tra7AlI2JPncLJTvHVJuGW4c=";
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

  cargoHash = "sha256-L5zufZZvJxwctSdaDWzWxdAd4zrjF0mNxCCXRlPETxs=";

  passthru.updateScript = ./update.sh;

  meta = with lib; {
    description = "A persistent browser terminal with a Rust server and xterm.js";
    homepage = "https://git.sr.ht/~sirn/fizzterm";
    license = licenses.mit;
    mainProgram = "fizzterm";
  };
}
