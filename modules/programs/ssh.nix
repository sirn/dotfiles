{ config, lib, pkgs, ... }:

{
  programs.ssh = {
    enable = true;
    package = pkgs.openssh;
    enableDefaultConfig = false;

    matchBlocks = {
      "*" = {
        compression = true;
        controlMaster = "auto";
        controlPath = "${config.home.homeDirectory}/.ssh/ssh-%r@%h:%p";
        controlPersist = "10m";
        serverAliveCountMax = 3;
        serverAliveInterval = 30;
        userKnownHostsFile = "${config.home.homeDirectory}/.ssh/known_hosts";

        extraOptions = {
          "CheckHostIP" = "yes";
          "StrictHostKeyChecking" = "accept-new";
          "PreferredAuthentications" = "publickey";
        };
      };
    };

    includes = [
      "${config.home.homeDirectory}/.ssh/config.d/*"
    ] ++ (if pkgs.stdenv.isDarwin then [
      "${config.home.homeDirectory}/.orbstack/ssh/config"
    ] else [ ]);
  };
}
