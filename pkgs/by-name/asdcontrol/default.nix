{
  lib,
  stdenv,
  fetchFromGitHub,
  clang,
  ...
}:

stdenv.mkDerivation rec {
  pname = "asdcontrol";
  version = "20250821";

  src = fetchFromGitHub {
    owner = "nikosdion";
    repo = pname;
    rev = "0ee8bd576d4e93513027d713e688988cb0d827ef";
    sha256 = "sha256-0BC1TKGRvCP62IhNmUcASZY+jzO1/O/Cupy0f7zqeBw=";
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
