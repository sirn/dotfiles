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

  CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_LINKER = "lld";
  HOME = "$TMPDIR/home";

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
