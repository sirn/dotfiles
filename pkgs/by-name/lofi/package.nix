{
  lib,
  rustPlatform,
  fetchgit,
}:

rustPlatform.buildRustPackage rec {
  pname = "lofi";
  version = "0.3.4";

  src = fetchgit {
    url = "https://github.com/sirn/lofi";
    rev = "refs/tags/v${version}";
    hash = "sha256-FtYboux2HeqWpQ3MPxGmkAPOSalKd0jtH1pdpI878aM=";
  };

  cargoHash = "sha256-YbhWfuMX+SwfkaRypBh0Er4On2BzW3UYPicXstfS/IU=";

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
