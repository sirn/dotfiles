{ stdenv, fetchurl, go_1_6 }:

stdenv.mkDerivation rec {
  name = "syncthing-${version}";
  version = "0.13.6";

  src = fetchurl {
    url = "https://github.com/syncthing/syncthing/archive/v${version}.tar.gz";
    sha256 = "1wzjmmgdd0mji1asbxsr59pn53n6rb6dkl12jnrys5xh7af7i6fz";
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