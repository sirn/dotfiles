{ lib, pkgs, ... }:

{
  home.packages = [
    (pkgs.unstable.claude-code.overrideDerivation (attrs: {
      # MCP typically requires npx/uvx/etc.; let's make sure that
      # nodejs/uv is always available to claude
      postInstall = attrs.postInstall + ''
        wrapProgram $out/bin/claude \
          --prefix PATH : ${pkgs.nodejs}/bin \
          --prefix PATH : ${pkgs.uv}/bin
      '';
    }))
  ];

  programs.git = {
    ignores = [
      ".claude/"
    ];
  };
}
