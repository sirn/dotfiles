{
  lib,
  stdenv,
  rustPlatform,
  fetchgit,
}:

rustPlatform.buildRustPackage rec {
  pname = "coord";
  version = "0.2.3";

  src = fetchgit {
    url = "https://git.sr.ht/~sirn/coord";
    rev = "refs/tags/v${version}";
    hash = "sha256-P4bJ7MFSAXAQi1ac3lP9IOFIJa7j5qwkWKkSvVguAvw=";
  };

  cargoHash = "sha256-cHZT/+GE0Fjr2RhmA7BHZU6s4wNOTZKRIx5+jTmjzcQ=";

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
