{ stdenv, fetchurl, pkgconfig, autoreconfHook, cacert
, openssl, c-ares, libxml2, sqlite, zlib, libssh2
}:

stdenv.mkDerivation rec {
  name = "aria2-${version}";
  version = "1.23.0";

  src = fetchurl {
    url = "https://github.com/tatsuhiro-t/aria2/releases/download/release-${version}/${name}.tar.xz";
    sha256 = "14qz7686zxnhbaqj6l1hqpkykhpygm74h2mzwhh13gqmcj38alaq";
  };

  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ openssl c-ares libxml2 sqlite zlib libssh2 ];

  configureFlags = [ "--with-ca-bundle=${cacert}/etc/ssl/certs/ca-bundle.crt" ] ++
    stdenv.lib.optional stdenv.isDarwin [ "--with-openssl" "--without-appletls" ];

  enableParallelBuilding = true;

  meta = with stdenv.lib; {
    homepage = https://github.com/tatsuhiro-t/aria2;
    description = "A lightweight, multi-protocol, multi-source, command-line download utility";
    maintainers = with maintainers; [ koral jgeerds ];
    license = licenses.gpl2Plus;
    platforms = platforms.unix;
  };
}
