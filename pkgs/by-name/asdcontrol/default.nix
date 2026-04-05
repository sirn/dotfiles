{
  lib,
  stdenv,
  fetchFromGitHub,
  clang,
  ...
}:

stdenv.mkDerivation rec {
  pname = "asdcontrol";
  version = "20240101";

  src = fetchFromGitHub {
    owner = "nikosdion";
    repo = pname;
    rev = "fb82248c3767711e5dbd004516bf8137bd7e6e09";
    sha256 = "sha256-195H+/ONsNOdQDXLq00Mfjlj/XGW6CZKnNi8mSIIEcE=";
  };

  buildInputs = [ clang ];

  buildPhase = ''
    make
  '';

  installPhase = ''
    install -d $out/bin
    install -m0755 asdcontrol $out/bin/asdcontrol
  '';
}
