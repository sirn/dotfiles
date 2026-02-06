{ lib, rustPlatform, fetchgit, pkg-config, openssl }:

rustPlatform.buildRustPackage rec {
  pname = "repoman";
  version = "0.2.0";

  src = fetchgit {
    url = "https://git.sr.ht/~sirn/repoman";
    rev = "refs/tags/v${version}";
    hash = "sha256-rAiTmE2KtGDzLDLtR1KbnV1DSPbw9awUFEf8Tnjze+8=";
  };

  cargoHash = "sha256-iKH1hHQwnkkqfHo+uVYYUTqla6Gm6H0YWVaFVOIgJkQ=";

  nativeBuildInputs = [ pkg-config ];

  buildInputs = [ openssl ];

  meta = with lib; {
    description = "Repository management tool";
    homepage = "https://git.sr.ht/~sirn/repoman";
    license = licenses.mit;
    mainProgram = "repoman";
  };
}
