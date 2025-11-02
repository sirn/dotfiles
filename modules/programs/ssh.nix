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
          "PreferredAuthentications" = "publickey";
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
    };

    userKnownHostsFile = "${config.home.homeDirectory}/.ssh/known_hosts";

    includes = [
      "${config.home.homeDirectory}/.ssh/config.d/*"
    ] ++ (if pkgs.stdenv.isDarwin then [
      "${config.home.homeDirectory}/.orbstack/ssh/config"
    ] else [ ]);
  };
}
