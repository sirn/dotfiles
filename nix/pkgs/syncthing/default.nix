{ stdenv, fetchurl, go_1_6 }:

stdenv.mkDerivation rec {
  name = "syncthing-${version}";
  version = "0.13.4";

  src = fetchurl {
    url = "https://github.com/syncthing/syncthing/archive/v${version}.tar.gz";
    sha256 = "0jzny1751m6p8y98v6dx2bj4kh9f7l9hnbf77yh9awl3wjnbgv67";
  };

  buildInputs = [ go_1_6 ];
  buildPhase = ''
    mkdir -p src/github.com/syncthing
    ln -s $(pwd) src/github.com/syncthing/syncthing
    export GOPATH=$(pwd)
    cd src/github.com/syncthing/syncthing
    go run build.go -no-upgrade -version v${version}
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp bin/syncthing $out/bin
  '';

  meta = with stdenv.lib; {
    description = "Open source continuous file synchronization";
    homepage = "https://syncthing.net/";
    license = licenses.mpl20;
    platforms = platforms.unix;
  };
}