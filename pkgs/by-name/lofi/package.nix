{
  lib,
  rustPlatform,
  fetchgit,
}:

rustPlatform.buildRustPackage rec {
  pname = "lofi";
  version = "0.2.4";

  src = fetchgit {
    url = "https://git.sr.ht/~sirn/lofi";
    rev = "refs/tags/v${version}";
    hash = "sha256-e1l0L7pLrRP9MM1omMItb9PBzUf1Rn4bysSIm4eAbbM=";
  };

  cargoHash = "sha256-b/z2eBZjrQd/0V0hK6mut9cIJ7miAYfjGUWgfK9koGA=";

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
