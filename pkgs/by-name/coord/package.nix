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
  version = "0.4.1";

  src = fetchgit {
    url = "https://git.sr.ht/~sirn/coord";
    rev = "refs/tags/v${version}";
    hash = "sha256-k0+70F7cMEgD+fQZUNZZI+CXBg+v8sAMZZzk0vBwRDw=";
  };

  cargoHash = "sha256-1ycisGMPBJtx1RlA+5kK/LRg3xcHGqhZuV9RV2dNogA=";

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
