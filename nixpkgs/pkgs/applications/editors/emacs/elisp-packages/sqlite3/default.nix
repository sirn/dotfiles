{ lib, stdenv, fetchFromGitHub, emacs, sqlite, writeText }:

stdenv.mkDerivation rec {
  pname = "sqlite3";
  version = "0.17";

  src = fetchFromGitHub {
    owner = "pekingduck";
    repo = "emacs-sqlite3-api";
    rev = "v${version}";
    sha256 = "sha256-nfJTV0bpqXMfTN3h6Fv9PwKLsg3jsj6i+/WQZlFAZvg=";
  };

  buildInputs = [ sqlite ];

  buildPhase = ''
    cd "$NIX_BUILD_TOP/source"
    make INC="${sqlite.dev}/include" LIB="-L${sqlite.out}/lib -lsqlite3"
  '';

  installPhase = ''
    install -d $out/share/emacs/site-lisp
    install -m644 *.so $out/share/emacs/site-lisp
    install -m644 *.el *.elc $out/share/emacs/site-lisp
  '';

  meta = with lib; {
    homepage = "https://github.com/pekingduck/emacs-sqlite3-api/";
    description = "SQLite3 API for GNU Emacs 25+";
    license = licenses.gpl3Only;
    maintainers = with maintainers; [ ];
    inherit (emacs.meta) platforms;
  };
}
