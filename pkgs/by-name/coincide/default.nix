{
  lib,
  buildWasmBindgenCli,
  binaryen,
  fetchCrate,
  fetchFromSourcehut,
  lld,
  openssl,
  pkg-config,
  rustPlatform,
  tailwindcss_4,
  trunk,
}:

let
  pname = "coincide";
  version = "0.1.3";
  wasm-bindgen-cli = buildWasmBindgenCli rec {
    src = fetchCrate {
      pname = "wasm-bindgen-cli";
      version = "0.2.120";
      hash = "sha256-Dkkx8Bhfk+y/jEz9Fzwytmv2N3Gj/7ST+5MlPRzzetU=";
    };
    cargoDeps = rustPlatform.fetchCargoVendor {
      inherit src;
      inherit (src) pname version;
      hash = "sha256-5Zu/Sh9aBMxB+KGC1MHWJAQ8PuE40M6lsenkpFEwJ6A=";
    };
  };
in
rustPlatform.buildRustPackage {
  inherit pname version;

  src = fetchFromSourcehut {
    owner = "~sirn";
    repo = "coincide";
    rev = "v${version}";
    hash = "sha256-MmmiwXElcyaGelGwgmA2JbKtzR2le2YPFrmWWlP0ZeU=";
  };

  buildFeatures = [ "embed-assets" ];

  cargoBuildFlags = [ "-p coincide-server" ];
  cargoInstallFlags = [ "-p coincide-server" ];
  cargoTestFlags = [
    "-p"
    "coincide-server"
  ];

  nativeBuildInputs = [
    binaryen
    lld
    pkg-config
    tailwindcss_4
    trunk
    wasm-bindgen-cli
  ];

  buildInputs = [ openssl ];

  CARGO_PROFILE_RELEASE_CODEGEN_UNITS = "1";
  CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_LINKER = "lld";
  HOME = "$TMPDIR/home";

  # The build embeds build-directory paths (panic locations) and a wasm-split
  # marker derived from CARGO_MANIFEST_PATH into the wasm, which rust-embed
  # then bakes into coincide-server. Remap the build top to a stable prefix so
  # the output is bit-for-bit reproducible across nix builds.
  preConfigure = ''
    export RUSTFLAGS="--remap-path-prefix=$NIX_BUILD_TOP=/build"
  '';

  # wasm_split_macros::version_stamp derives a no_mangle-collision marker from
  # sha256(CARGO_MANIFEST_PATH), which varies per build directory. Derive it
  # from the stable crate name instead.
  postPatch = ''
    f="$cargoDepsCopy/source-registry-0/wasm_split_macros-0.2.1/src/lib.rs"
    substituteInPlace "$f" \
      --replace-fail 'std::env::var_os("CARGO_MANIFEST_PATH").unwrap()' 'std::env::var("CARGO_PKG_NAME").unwrap()' \
      --replace-fail 'unique_path.as_encoded_bytes()' 'unique_path.as_bytes()'
  '';

  preBuild = ''
    cd coincide-app
    trunk build --offline --frozen --release
    cd ..
  '';

  cargoHash = "sha256-LXxQhk5Y4YTK0/owVGOlMclyCQ+saLJznPTyGzgwuik=";

  meta = with lib; {
    description = "Local web app for reviewing Git commits and Jujutsu changes in an inline diff view";
    homepage = "https://git.sr.ht/~sirn/coincide";
    license = licenses.bsd3;
    mainProgram = "coincide-server";
  };
}
