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

    matchBlocks =
      let
        insertBefore = lib.hm.dag.entryBefore [ "*" "console" ];
      in
      {
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

        "bitbucket.org" = insertBefore {
          user = "git";
          extraOptions = {
            "CheckHostIP" = "no";
          };
        };

        "github.com" = insertBefore {
          user = "git";
          extraOptions = {
            "CheckHostIP" = "no";
          };
        };

        "gitlab.com" = insertBefore {
          user = "git";
          extraOptions = {
            "CheckHostIP" = "no";
          };
        };

        "git.sr.ht" = insertBefore {
          user = "git";
          extraOptions = {
            "CheckHostIP" = "no";
          };
        };

        "list-*.linode.com" = insertBefore {
          extraOptions = {
            "CheckHostIP" = "no";
          };
        };
      };

    userKnownHostsFile = "${config.home.homeDirectory}/.ssh/known_hosts";

    includes = [
      "${config.home.homeDirectory}/.ssh/config.d/*"
    ] ++ (if pkgs.stdenv.isDarwin then [
      "${config.home.homeDirectory}/.orbstack/ssh/config"
    ] else [ ]);
  };
}
