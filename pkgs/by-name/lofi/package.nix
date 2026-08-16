{
  lib,
  rustPlatform,
  fetchgit,
}:

rustPlatform.buildRustPackage rec {
  pname = "lofi";
  version = "0.2.2";

  src = fetchgit {
    url = "https://git.sr.ht/~sirn/lofi";
    rev = "refs/tags/v${version}";
    hash = "sha256-7SWlNPS5JBiu9/PYABlGagCCZHzJ3R/ZcXjXDKwWLVw=";
  };

  cargoHash = "sha256-Bz1+nLUQh8n3vs81W25hewuO2QTigtj7rGWcLzTG4QY=";

  cargoBuildFlags = [
    "-p"
    "lofi"
  ];

  doCheck = false;

  meta = with lib; {
    description = "Minimal coding-agent harness written in Rust";
    homepage = "https://git.sr.ht/~sirn/lofi";
    license = licenses.asl20;
    mainProgram = "lofi";
    platforms = platforms.linux ++ platforms.darwin;
  };
}
