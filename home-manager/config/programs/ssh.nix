{
  config,
  lib,
  pkgs,
  ...
}:

{
  programs.ssh = {
    enable = true;
    package = pkgs.openssh;
    enableDefaultConfig = false;

    settings = {
      "*" = {
        Compression = "yes";
        ControlMaster = "auto";
        ControlPath = "${config.home.homeDirectory}/.ssh/ssh-%r@%h:%p";
        ControlPersist = "10m";
        ServerAliveCountMax = 3;
        ServerAliveInterval = 60;
        UserKnownHostsFile = "${config.home.homeDirectory}/.ssh/known_hosts";
        CheckHostIP = "yes";
        PreferredAuthentications = "publickey";
        StrictHostKeyChecking = "accept-new";
        TCPKeepAlive = "no";
      };
    };

    includes = [
      "${config.home.homeDirectory}/.ssh/config.d/*"
    ]
    ++ (if pkgs.stdenv.isDarwin then [ "${config.home.homeDirectory}/.orbstack/ssh/config" ] else [ ]);
  };
}
