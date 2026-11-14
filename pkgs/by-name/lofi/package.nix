{
  lib,
  rustPlatform,
  fetchgit,
}:

rustPlatform.buildRustPackage rec {
  pname = "lofi";
  version = "0.3.7";

  src = fetchgit {
    url = "https://github.com/cakework/lofi";
    rev = "refs/tags/v${version}";
    hash = "sha256-7LiUgnkZo0/jYW2q9DahEShg93KRvtEPSZjwd7btz80=";
  };

  cargoHash = "sha256-5KyMq83uYtylaUsjBJoSPvd6fN66JusJp6NBxonLAE0=";

  cargoBuildFlags = [
    "-p"
    "lofi"
  ];

  doCheck = false;

  meta = with lib; {
    description = "Minimal coding-agent harness written in Rust";
    homepage = "https://github.com/cakework/lofi";
    license = licenses.asl20;
    mainProgram = "lofi";
    platforms = platforms.linux ++ platforms.darwin;
  };
}
