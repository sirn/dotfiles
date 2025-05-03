{ config, pkgs, ... }:

let
  dotprivDir = "${config.home.homeDirectory}/.dotpriv";
in
{
  home.packages = [
    pkgs.local.claude-code
  ];

  programs.git = {
    ignores = [
      "CLAUDE.local.md"
    ];
  };

  home.file = {
    ".claude/CLAUDE.md" = { 
      source = config.lib.file.mkOutOfStoreSymlink "${dotprivDir}/etc/claude/CLAUDE.md";
    };
  };
}
