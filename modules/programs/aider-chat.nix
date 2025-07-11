{ config, lib, pkgs, ... }:

{
  home.packages = [
    pkgs.unstable.aider-chat
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

        # This file will require importing home/llm-agent.nix
        read = [
          "${config.home.homeDirectory}/.local/var/AGENTS.md"
        ];

        # Disable Git unless explicitly enabled since Aider will nag
        # about creating Git otherwise (I'm using jj...)
        git = false;

        # Don't auto commit.
        auto-commits = false;
        dirty-commits = false;
      };
    };

    ".aider/oauth-keys.env" = {
      source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.env";
    };
  };
}
