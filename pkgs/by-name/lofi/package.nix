{
  lib,
  rustPlatform,
  fetchgit,
}:

rustPlatform.buildRustPackage rec {
  pname = "lofi";
  version = "0.3.8";

  src = fetchgit {
    url = "https://github.com/cakework/lofi";
    rev = "refs/tags/v${version}";
    hash = "sha256-nbMFeVAbIWNZhHAeDT4lGTnieR14jPpWcbJQSBkOUoc=";
  };

  cargoHash = "sha256-1MQGJi1zGBwVwL7+seJjadO6lS77+tJsWZWgvlQF25Q=";

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
