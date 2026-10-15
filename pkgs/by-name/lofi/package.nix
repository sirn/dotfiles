{
  lib,
  rustPlatform,
  fetchgit,
}:

rustPlatform.buildRustPackage rec {
  pname = "lofi";
  version = "0.3.0";

  src = fetchgit {
    url = "https://github.com/sirn/lofi";
    rev = "refs/tags/v${version}";
    hash = "sha256-N2GRdQ/YESKLnKUdbdqWOyUglEGu3gESrVAWOX1csVc=";
  };

  cargoHash = "sha256-0NUxJkI2tKSJAv80QxXCzH5147+MOMaWoowZR/mjJCA=";

  cargoBuildFlags = [
    "-p"
    "lofi"
  ];

  doCheck = false;

  meta = with lib; {
    description = "Minimal coding-agent harness written in Rust";
    homepage = "https://github.com/sirn/lofi";
    license = licenses.asl20;
    mainProgram = "lofi";
    platforms = platforms.linux ++ platforms.darwin;
  };
}
