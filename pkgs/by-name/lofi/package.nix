{
  lib,
  rustPlatform,
  fetchgit,
}:

rustPlatform.buildRustPackage rec {
  pname = "lofi";
  version = "0.3.5";

  src = fetchgit {
    url = "https://github.com/sirn/lofi";
    rev = "refs/tags/v${version}";
    hash = "sha256-wbhWYqSnpgQ0XaUE3lsswYU3zlBKw3yVyUfYnPbYLeM=";
  };

  cargoHash = "sha256-u2m+L3NY7qLfYyrtxqPAbO3Uut8WtBCCJovVeo2i7hs=";

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
