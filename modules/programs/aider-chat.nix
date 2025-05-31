{ lib, pkgs, ... }:

{
  home.packages = [
    pkgs.local.aider-chat
  ];

  programs.git = {
    ignores = [
      ".aider*"
    ];
  };

  home.file = {
    ".aider.conf.yml" = {
      source = (pkgs.formats.yaml { }).generate "aider.conf.yml" {
        dark-mode = true;
        git = false;
        auto-commits = false;
        dirty-commits = false;
      };
    };
  };
}
