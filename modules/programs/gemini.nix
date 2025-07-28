{ lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    (pkgs.unstable.gemini-cli.overrideDerivation (attrs: {
      postInstall = attrs.postInstall + ''
        wrapProgram $out/bin/gemini \
          --prefix PATH : ${pkgs.nodejs}/bin \
          --prefix PATH : ${pkgs.local.wrapped-uv}/bin
      '';
    }))
  ];

  programs.git = {
    ignores = [
      ".gemini/"
    ];
  };
}
