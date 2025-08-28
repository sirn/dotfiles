{ pkgs, ... }:

let
  package = pkgs.unstable.codex;
in
{
  home.packages = with pkgs; [
    (pkgs.stdenv.mkDerivation {
      pname = "wrapped-${package.name}";
      src = ./.;
      version = package.version;

      nativeBuildInputs = [
        pkgs.makeWrapper
      ];

      installPhase = ''
        mkdir -p $out/bin

        makeWrapper ${package}/bin/codex $out/bin/codex \
          --prefix PATH : ${pkgs.bun}/bin \
          --prefix PATH : ${pkgs.nodejs}/bin \
          --prefix PATH : ${pkgs.ripgrep}/bin \
          --prefix PATH : ${pkgs.local.wrapped-uv}/bin
      '';
    })
  ];

  programs.git = {
    ignores = [
      ".codex/"
    ];
  };
}
