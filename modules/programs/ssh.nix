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
    };

    userKnownHostsFile = "${config.home.homeDirectory}/.ssh/known_hosts";

    includes = [
      "${config.home.homeDirectory}/.ssh/config.d/*"
    ] ++ (if pkgs.stdenv.isDarwin then [
      "${config.home.homeDirectory}/.orbstack/ssh/config"
    ] else [ ]);
  };
}
