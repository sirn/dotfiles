{ lib, rustPlatform, fetchgit, pkg-config, openssl }:

rustPlatform.buildRustPackage rec {
  pname = "repoman";
  version = "0.3.2";

  src = fetchgit {
    url = "https://git.sr.ht/~sirn/repoman";
    rev = "refs/tags/v${version}";
    hash = "sha256-FM/6q2f6aR2c0AhcBmvMZ2+UJNlD1YhUYziwY7pb+cI=";
  };

  cargoHash = "sha256-JkvX5Nu/neBDKN464U4QE9+0e28ksnqg9k7LthzCeMM=";

  nativeBuildInputs = [ pkg-config ];

  buildInputs = [ openssl ];

  meta = with lib; {
    description = "Repository management tool";
    homepage = "https://git.sr.ht/~sirn/repoman";
    license = licenses.mit;
    mainProgram = "repoman";
  };
}
