{
  lib,
  rustPlatform,
  fetchgit,
}:

rustPlatform.buildRustPackage rec {
  pname = "lofi";
  version = "0.1.0";

  src = fetchgit {
    url = "https://git.sr.ht/~sirn/lofi";
    rev = "refs/tags/v${version}";
    hash = "sha256-dlrHjfNHl7V1AmCEY/aG6jULu7vXHd0WQusv8HOMgV0=";
  };

  cargoHash = "sha256-18QmRYqVpk8hPI2glttBPsVRU556VVSIqNvq0ru98jQ=";

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
