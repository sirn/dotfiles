{
  lib,
  rustPlatform,
  fetchgit,
}:

rustPlatform.buildRustPackage rec {
  pname = "zrex";
  version = "0.3.0";

  src = fetchgit {
    url = "https://github.com/sirn/zrex";
    rev = "refs/tags/v${version}";
    hash = "sha256-FF9BCHDcUMUNfRQ4PdJaHJqKiR7ucvvEfqQ8SCOjBKI=";
  };

  cargoHash = "sha256-d8+9pCKCfmMRynwTuk0hZtIFBkaoivwVyKk1kFNfuaE=";

  meta = with lib; {
    description = "Command-line client for a Rex server";
    homepage = "https://github.com/sirn/zrex";
    license = licenses.mit;
    mainProgram = "zrex";
    platforms = platforms.unix;
  };
}
