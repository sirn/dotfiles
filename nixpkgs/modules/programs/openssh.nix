{ config, lib, pkgs, ... }:

let
  inherit (config.home) homeDirectory;
  inherit (pkgs.stdenv) isDarwin;

  dotprivDir = "${homeDirectory}/.dotpriv";
in
{
  programs.ssh = {
    enable = true;
    package = pkgs.openssh;

    compression = true;
    controlMaster = "auto";
    controlPath = "${homeDirectory}/.ssh/ssh-%r@%h:%p";
    controlPersist = "10m";
    serverAliveCountMax = 3;
    serverAliveInterval = 30;

    matchBlocks = {
      "*" = {
        user = "sirn";
        extraOptions = {
          "CheckHostIP" = "yes";
          "StrictHostKeyChecking" = "yes";
          "PreferredAuthentications" = "publickey";
        };
      };
    };

    userKnownHostsFile = lib.concatStringsSep " " [
      "${homeDirectory}/.ssh/known_hosts"
      "${homeDirectory}/.ssh/known_hosts2"
      "${dotprivDir}/etc/ssh/known_hosts"
    ];

    includes = [
      "${dotprivDir}/etc/ssh/config.d/*"
    ] ++ (if isDarwin then [
      "${homeDirectory}/.orbstack/ssh/config"
    ] else [ ]);
  };
}
