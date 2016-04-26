{ stdenv, lib, fetchurl
, AppKit, Cocoa, ScriptingBridge
}:

stdenv.mkDerivation rec {
  name = "trash-${version}";
  version = "0.8.5";

  src = fetchurl {
    url = "https://github.com/ali-rantakari/trash/archive/v${version}.tar.gz";
    sha256 = "16df7b2fa3431k1nhb1cws0qsf23b4lmgwvvxqdbw5m2xb6zs20y";
  };

  buildInputs = [ AppKit Cocoa ScriptingBridge ];
  buildPhase = ''
    clang -O2 -Wall -Wextra -framework AppKit -framework ScriptingBridge -o trash trash.m HGUtils.m HGCLIUtils.m fileSize.m
  '';

  installPhase = ''
    mkdir -p $out/bin/
    cp trash $out/bin/
  '';

  meta = {
    description = "Moves files or folders to the trash";
    homepage = "http://hasseg.org/trash/";
    maintainers = [];
    license =  "MIT";
    platforms = lib.platforms.darwin;
  };
}
