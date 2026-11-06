{
  lib,
  rustPlatform,
  fetchgit,
}:

rustPlatform.buildRustPackage rec {
  pname = "zrex";
  version = "0.1.0";

  src = fetchgit {
    url = "https://github.com/sirn/zrex";
    rev = "refs/tags/v${version}";
    hash = "sha256-VMaRi+pYFC6qivxzBLf0No5412UJmcEJEpyXY5O2tDc=";
  };

  cargoHash = "sha256-AO+5PrNKSR6yKfb+t5JqiIgOj++nTQ9th2sfWNZIipw=";

  meta = with lib; {
    description = "Command-line client for a Rex server";
    homepage = "https://github.com/sirn/zrex";
    license = licenses.mit;
    mainProgram = "zrex";
    platforms = platforms.unix;
  };
}
