{ config, lib, pkgs, ... }:

{
  programs.ssh = {
    enable = true;
    package = pkgs.openssh;

    compression = true;
    controlMaster = "auto";
    controlPath = "${config.home.homeDirectory}/.ssh/ssh-%r@%h:%p";
    controlPersist = "10m";
    serverAliveCountMax = 3;
    serverAliveInterval = 30;

    matchBlocks = {
      "*" = {
        extraOptions = {
          "CheckHostIP" = "yes";
          "StrictHostKeyChecking" = "accept-new";
          "PreferredAuthentications" = "publickey,keyboard-interactive,password";
        };
      };

      # This is used for when 1Password agent or Bitwarden agent is enabled;
      # we want to fallback to default agent when we're not running under
      # X or Wayland.
      "console" = lib.mkIf pkgs.stdenv.isLinux (lib.hm.dag.entryBefore [ "*" ] {
        match = "exec \"test -z \\\"$DISPLAY\\\" && test -z \\\"$WAYLAND_DISPLAY\\\"\"";
        extraOptions = {
          "IdentityAgent" = "$SSH_AUTH_SOCK";
        };
      });

      "bitbucket.org" = lib.hm.dag.entryBefore [ "*" "console" ] {
        user = "git";
        extraOptions = {
          "CheckHostIP" = "no";
        };
      };

      "github.com" = lib.hm.dag.entryBefore [ "*" "console" ] {
        user = "git";
        extraOptions = {
          "CheckHostIP" = "no";
        };
      };

      "gitlab.com" = lib.hm.dag.entryBefore [ "*" "console" ] {
        user = "git";
        extraOptions = {
          "CheckHostIP" = "no";
        };
      };

      "git.sr.ht" = lib.hm.dag.entryBefore [ "*" "console" ] {
        user = "git";
        extraOptions = {
          "CheckHostIP" = "no";
        };
      };

      "list-*.linode.com" = lib.hm.dag.entryBefore [ "*" "console" ] {
        extraOptions = {
          "CheckHostIP" = "no";
        };
      };
    };

    userKnownHostsFile = lib.concatStringsSep " " [
      "${config.home.homeDirectory}/.ssh/known_hosts"
      "${config.home.homeDirectory}/.ssh/known_hosts2"
    ];

    includes = [
      "${config.home.homeDirectory}/.ssh/config.d/*"
    ] ++ (if pkgs.stdenv.isDarwin then [
      "${config.home.homeDirectory}/.orbstack/ssh/config"
    ] else [ ]);
  };
}
