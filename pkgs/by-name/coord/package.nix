{ lib, rustPlatform, fetchgit }:

rustPlatform.buildRustPackage rec {
  pname = "coord";
  version = "0.1.0";

  src = fetchgit {
    url = "https://git.sr.ht/~sirn/coord";
    rev = "refs/tags/v${version}";
    hash = "sha256-Kel//99/uc5xCo0D9+olNMdbgwkj8UUt/jroDglTHmU=";
  };

  cargoHash = "sha256-fd9TGHRPnO6W0h6ARaWMKXx+KlnnV/43Tp0Jr6rl+e8=";

  cargoBuildFlags = [ "-p" "coord" ];
  doCheck = false;

  meta = with lib; {
    description = "Keyboard-controlled mouse for Wayland";
    homepage = "https://git.sr.ht/~sirn/coord";
    license = licenses.mit;
    mainProgram = "coord";
    platforms = platforms.linux;
  };
}
