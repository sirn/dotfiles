{ lib, rustPlatform, fetchgit, pkg-config, openssl }:

rustPlatform.buildRustPackage rec {
  pname = "repoman";
  version = "0.1.2";

  src = fetchgit {
    url = "https://git.sr.ht/~sirn/repoman";
    rev = "refs/tags/v${version}";
    hash = "sha256-X6LUigayjBLpxyoexRO/UPJs2PVxjYhTGiMhykRzlDo=";
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
