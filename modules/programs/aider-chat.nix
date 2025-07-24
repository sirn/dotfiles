{ config, lib, pkgs, ... }:

let
  uvxAiderChat = pkgs.writeScriptBin "aider" ''
    #!${pkgs.bash}/bin/bash
    # Runs Aider from uvx
    exec ${pkgs.uv}/bin/uvx --managed-python --python 3.12 --from aider-chat aider "$@"
  '';

  fhsAiderChat = pkgs.buildFHSEnv {
    name = "aider";
    targetPkgs = pkgs: with pkgs; [
      uvxAiderChat

      # Used by uvx
      openssl
      pkg-config
      stdenv.cc.cc
      zlib

      # For dynamically linked binaries
      nix-ld
    ];

    runScript = "aider";
  };
in
{
  home.packages = [
    (if pkgs.stdenv.isDarwin
    then uvxAiderChat
    else fhsAiderChat)
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
