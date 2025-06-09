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

        # Disable Git unless explicitly enabled since Aider will nag
        # about creating Git otherwise (I'm using jj...)
        no-git = true;

        # Don't auto commit.
        auto-commits = false;
        dirty-commits = false;
      };
    };
  };
}
