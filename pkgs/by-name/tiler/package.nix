{
  lib,
  stdenv,
  swift,
  swiftpm,
  fetchgit,
  cacert,
  git,
}:

stdenv.mkDerivation rec {
  pname = "tiler";
  version = "0.5.4";

  src = fetchgit {
    url = "https://github.com/sirn/tiler";
    rev = "refs/tags/v${version}";
    hash = "sha256-qI0XNc/NsfQ9lks2N9sII1EnEF1AJA07pacbpGQRW+E=";
  };

  nativeBuildInputs = [
    swift
    swiftpm
    git
  ];
  buildInputs = [ cacert ];

  GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";

  buildPhase = ''
    runHook preBuild
    swift build -c release
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    app=$out/Applications/TilerApp.app
    mkdir -p $app/Contents/MacOS $app/Contents/Resources
    cp .build/release/TilerApp $app/Contents/MacOS/
    cp Resources/Info.plist $app/Contents/
    cp Resources/config.toml.example $app/Contents/Resources/
    runHook postInstall
  '';

  meta = with lib; {
    description = "Niri-like WM for macOS";
    homepage = "https://github.com/sirn/tiler";
    license = licenses.mit;
    mainProgram = "TilerApp";
    platforms = platforms.darwin;
  };
}
