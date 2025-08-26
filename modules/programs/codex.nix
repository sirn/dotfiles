{ pkgs, ... }:

{
  home.packages = with pkgs; [
    (pkgs.unstable.codex.overrideDerivation (attrs: {
      nativeBuildInputs = (attrs.nativeBuildInputs or [ ]) ++ [ pkgs.makeWrapper ];

      postInstall = (attrs.postInstall or "") + ''
        wrapProgram $out/bin/codex \
          --prefix PATH : ${pkgs.bun}/bin \
          --prefix PATH : ${pkgs.nodejs}/bin \
          --prefix PATH : ${pkgs.ripgrep}/bin \
          --prefix PATH : ${pkgs.local.wrapped-uv}/bin
      '';
    }))
  ];

  programs.git = {
    ignores = [
      ".codex/"
    ];
  };
}
