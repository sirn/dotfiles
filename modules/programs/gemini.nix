{ pkgs, ... }:

{
  home.packages = with pkgs; [
    (pkgs.unstable.gemini-cli.overrideDerivation (attrs: {
      nativeBuildInputs = (attrs.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];

      postInstall = attrs.postInstall + ''
        wrapProgram $out/bin/gemini \
          --prefix PATH : ${pkgs.bun}/bin \
          --prefix PATH : ${pkgs.nodejs}/bin \
          --prefix PATH : ${pkgs.ripgrep}/bin \
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
