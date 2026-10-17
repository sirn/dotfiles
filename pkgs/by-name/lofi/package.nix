{
  lib,
  rustPlatform,
  fetchgit,
}:

rustPlatform.buildRustPackage rec {
  pname = "lofi";
  version = "0.3.1";

  src = fetchgit {
    url = "https://github.com/sirn/lofi";
    rev = "refs/tags/v${version}";
    hash = "sha256-ytNwbODO/BYWQ4cSyjfbD+EVSDC/2pOZqzVb3O95s5g=";
  };

  cargoHash = "sha256-wJ0qPNPuaHHU2ubF67oRNEZUtZkJ66FST7awUV8mK1A=";

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
