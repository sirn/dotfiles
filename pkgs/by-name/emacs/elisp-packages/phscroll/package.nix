{
  lib,
  stdenv,
  fetchFromGitHub,
  emacs,
}:

stdenv.mkDerivation {
  pname = "phscroll";
  version = "unstable-2025-02-25";

  src = fetchFromGitHub {
    owner = "misohena";
    repo = "phscroll";
    rev = "582abedb4cf6aba216cdb5fe7217d612a1d68d5a";
    sha256 = "0cp0ffgdg6livjyb36m54mai0apwr2qf80v32x92v76q00542j9f";
  };

  buildInputs = [ emacs ];

  installPhase = ''
    install -d $out/share/emacs/site-lisp
    install -m644 *.el $out/share/emacs/site-lisp/
  '';

  meta = with lib; {
    homepage = "https://github.com/misohena/phscroll";
    description = "Horizontal scroll for Emacs";
    license = licenses.gpl3Plus;
    inherit (emacs.meta) platforms;
  };
}
