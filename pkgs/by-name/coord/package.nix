{ lib, rustPlatform, fetchgit }:

rustPlatform.buildRustPackage rec {
  pname = "coord";
  version = "0.2.0";

  src = fetchgit {
    url = "https://git.sr.ht/~sirn/coord";
    rev = "refs/tags/v${version}";
    hash = "sha256-RxC4YE6WscwQky1RVe3TYJ3iv42cXlmBH2OSldsHYZc=";
  };

  cargoHash = "sha256-d75ECLxv8XwmLrw0vGjcxcX1fiIDCcNN6QEZhGwsVlQ=";

  cargoBuildFlags = [ "-p" "coord" ];
  doCheck = false;

  meta = with lib; {
    description = "Keyboard-controlled mouse for Wayland";
    homepage = "https://git.sr.ht/~sirn/coord";
    license = licenses.mit;
    mainProgram = "coord";
    platforms = platforms.linux ++ platforms.darwin;
  };
}
