{ pkgs, emacsPackages, ... }:

emacsPackages.visual-regexp-steroids.overrideDerivation (attrs: {
  postPatch = ''
    substituteInPlace visual-regexp-steroids.el \
      --replace "python %s" "${pkgs.python311}/bin/python3 %s"
  '';
})
