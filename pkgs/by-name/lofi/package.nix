{
  lib,
  rustPlatform,
  fetchgit,
}:

rustPlatform.buildRustPackage rec {
  pname = "lofi";
  version = "0.2.0";

  src = fetchgit {
    url = "https://git.sr.ht/~sirn/lofi";
    rev = "refs/tags/v${version}";
    hash = "sha256-/JFP6kIwzdP5DtnkybxaakR8IsnXfSADvQ1ddZtrVqc=";
  };

  cargoHash = "sha256-UdITC05vV13EkWupnbWiwTXBPApYbqlhBg9Oc/L4DLg=";

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
