{
  lib,
  stdenv,
  rustPlatform,
  fetchgit,
}:

rustPlatform.buildRustPackage rec {
  pname = "coord";
  version = "0.3.0";

  src = fetchgit {
    url = "https://git.sr.ht/~sirn/coord";
    rev = "refs/tags/v${version}";
    hash = "sha256-n0v0uzr1/E2xA4EAQHWbOaKvDvrOt41v7/D19L888lQ=";
  };

  cargoHash = "sha256-Y+hjer6fAKndVsjA45NOex2NWXZn4HR5mr39AlJFm5Q=";

  cargoBuildFlags = [
    "-p"
    "coord"
  ];

  doCheck = false;

  postInstall = lib.optionalString stdenv.isDarwin ''
    app=$out/Applications/Coord.app
    mkdir -p $app/Contents/MacOS $app/Contents/Resources
    cp $out/bin/coord $app/Contents/MacOS/
    cp contrib/macos/Info.plist $app/Contents/
    substituteInPlace $app/Contents/Info.plist \
      --replace "@VERSION@" "${version}"
  '';

  meta = with lib; {
    description = "Keyboard-controlled mouse for Wayland";
    homepage = "https://git.sr.ht/~sirn/coord";
    license = licenses.mit;
    mainProgram = "coord";
    platforms = platforms.linux ++ platforms.darwin;
  };
}
