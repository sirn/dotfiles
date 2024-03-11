{ pkgs, emacsPackages, ... }:

emacsPackages.apheleia.overrideDerivation (attrs: {
  nativeBuildInputs = (attrs.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];

  postInstall = ''
    wrapProgram $out/share/emacs/site-lisp/elpa/${attrs.pname}-${attrs.version}/scripts/formatters/apheleia-npx \
      --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.nodePackages.prettier ]}
    wrapProgram $out/share/emacs/site-lisp/elpa/${attrs.pname}-${attrs.version}/scripts/formatters/apheleia-phpcs \
      --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.php83Packages.phpcs ]}
  '';
})
