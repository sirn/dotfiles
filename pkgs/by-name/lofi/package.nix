{
  lib,
  rustPlatform,
  fetchgit,
}:

rustPlatform.buildRustPackage rec {
  pname = "lofi";
  version = "0.3.2";

  src = fetchgit {
    url = "https://github.com/sirn/lofi";
    rev = "refs/tags/v${version}";
    hash = "sha256-6WLPdlu9t0QKdteGze6E8O5lAVL8yWbrviFiV9SMa5k=";
  };

  cargoHash = "sha256-3hBtzAZ0oPXjBQ0rEvh2rNAJtl/2WCAcTfU9keKh2l8=";

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
