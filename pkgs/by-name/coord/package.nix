{
  lib,
  stdenv,
  rustPlatform,
  fetchgit,
  libxkbcommon,
  pkg-config,
}:

rustPlatform.buildRustPackage rec {
  pname = "coord";
  version = "0.4.3";

  src = fetchgit {
    url = "https://git.sr.ht/~sirn/coord";
    rev = "refs/tags/v${version}";
    hash = "sha256-hzEoWN2fyRRmNPPdm33cBupolo/6pMBJEb45oxHo+cs=";
  };

  cargoHash = "sha256-CZgNmFEEAQC3lQKXlGMNg7tQhhXoD8lcQUw+A81aIdY=";

  nativeBuildInputs = [ pkg-config ];

  buildInputs = [ libxkbcommon ];

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
