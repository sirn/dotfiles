{
  lib,
  binaryen,
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
  version = "0.1.1";
in
rustPlatform.buildRustPackage {
  inherit pname version;

  src = fetchFromSourcehut {
    owner = "~sirn";
    repo = "coincide";
    rev = "v${version}";
    hash = "sha256-rvg+9ljy+CeUSBJ+il9KiJ6ifdhdgFFjeFyAWO0Y0w8=";
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
  ];

  buildInputs = [ openssl ];

  CARGO_TARGET_WASM32_UNKNOWN_UNKNOWN_LINKER = "lld";
  HOME = "$TMPDIR/home";

  preBuild = ''
    cd coincide-app
    trunk build --release
    cd ..
  '';

  cargoHash = "sha256-bM/oYhMtX4cOv1883svl5dKUfYFE1B4+vVGFKGm8UVg=";

  meta = with lib; {
    description = "Local web app for reviewing Git commits and Jujutsu changes in an inline diff view";
    homepage = "https://git.sr.ht/~sirn/coincide";
    license = licenses.bsd3;
    mainProgram = "coincide-server";
  };
}
