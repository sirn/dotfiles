{
  lib,
  rustPlatform,
  fetchgit,
}:

rustPlatform.buildRustPackage rec {
  pname = "lofi";
  version = "0.2.5";

  src = fetchgit {
    url = "https://git.sr.ht/~sirn/lofi";
    rev = "refs/tags/v${version}";
    hash = "sha256-iMokPhM1LLJ34oH7ziTmKp7M5s3WUrSuAhCFBKkEmQE=";
  };

  cargoHash = "sha256-bm7L7pWtoY5NHz50tX4Y6zKzZsTF8ONs0AFa7hP8hAU=";

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
