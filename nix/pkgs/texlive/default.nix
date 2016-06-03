{ stdenv, texlive }:

texlive.combine {
  inherit (texlive) scheme-small collection-latexrecommended;
}