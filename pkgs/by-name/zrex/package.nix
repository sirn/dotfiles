{
  lib,
  rustPlatform,
  fetchgit,
}:

rustPlatform.buildRustPackage rec {
  pname = "zrex";
  version = "0.2.0";

  src = fetchgit {
    url = "https://github.com/sirn/zrex";
    rev = "refs/tags/v${version}";
    hash = "sha256-Qo++exalPK/roenAw3Drtw730C/0SZYrtFdj0sdMbI0=";
  };

  cargoHash = "sha256-wwrd1qiF9ljuXvM5c6Yp0FoEwNxS1OkRln/vRZGkJso=";

  meta = with lib; {
    description = "Command-line client for a Rex server";
    homepage = "https://github.com/sirn/zrex";
    license = licenses.mit;
    mainProgram = "zrex";
    platforms = platforms.unix;
  };
}
