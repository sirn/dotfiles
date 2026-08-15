{
  lib,
  fetchFromSourcehut,
  rustPlatform,
}:

rustPlatform.buildRustPackage rec {
  pname = "powerband";
  version = "0.1.1";

  src = fetchFromSourcehut {
    owner = "~sirn";
    repo = "powerband";
    rev = "v${version}";
    hash = "sha256-a2GTzu9D+SiPx/4ygxcEbt9+0Y6rkZmcVxNnC3VkkuU=";
  };

  cargoHash = "sha256-jrc6tmWQCCayB2OshOmPTR/k4q146chVk2l16nwVvd4=";

  meta = with lib; {
    description = "Load-aware CPU power settings daemon (EPP, EPB, governor)";
    homepage = "https://git.sr.ht/~sirn/powerband";
    license = licenses.bsd3;
    mainProgram = "powerband";
  };
}
