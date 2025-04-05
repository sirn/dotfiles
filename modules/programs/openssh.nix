{ config, lib, pkgs, ... }:

let
  dotprivDir = "${config.home.homeDirectory}/.dotpriv";
in
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
        user = "sirn";
        extraOptions = {
          "CheckHostIP" = "yes";
          "StrictHostKeyChecking" = "accept-new";
          "PreferredAuthentications" = "publickey,keyboard-interactive,password";
        };
      };

      "bitbucket.org" = {
        user = "git";
        extraOptions = {
          "CheckHostIP" = "no";
        };
      };

      "github.com" = {
        user = "git";
        extraOptions = {
          "CheckHostIP" = "no";
        };
      };

      "gitlab.com" = {
        user = "git";
        extraOptions = {
          "CheckHostIP" = "no";
        };
      };

      "git.sr.ht" = {
        user = "git";
        extraOptions = {
          "CheckHostIP" = "no";
        };
      };

      "list-*.linode.com" = {
        extraOptions = {
          "CheckHostIP" = "no";
        };
      };

      # Aliases
      "nfs" = {
        hostname = "ssh.nyc1.nearlyfreespeech.net";
      };
    };

    userKnownHostsFile = lib.concatStringsSep " " [
      "${config.home.homeDirectory}/.ssh/known_hosts"
      "${config.home.homeDirectory}/.ssh/known_hosts2"
      "${dotprivDir}/etc/ssh/known_hosts"
    ];

    includes = [
      "${dotprivDir}/etc/ssh/config.d/*"
    ] ++ (if pkgs.stdenv.isDarwin then [
      "${config.home.homeDirectory}/.orbstack/ssh/config"
    ] else [ ]);
  };
}
