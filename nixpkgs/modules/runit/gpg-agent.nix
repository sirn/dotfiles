{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isLinux;
  inherit (config.home) username;

  homeDir = config.home.homeDirectory;
in
{
  runit.services = {
    gpg-agent = {
      runScript = ''
        #!${pkgs.execline}/bin/execlineb
        emptyenv -p
        export PATH ${pkgs.execline}/bin:${pkgs.busybox}/bin
        export HOME ${homeDir}

        backtick -n -E uid { id -u }
        define xdg-runtime-dir /run/user/''${uid}

        ${pkgs.s6}/bin/s6-ipcserver-socketbinder -a 0600 ''${xdg-runtime-dir}/gnupg/S.gpg-agent
        fdmove 3 0
        ${pkgs.s6}/bin/s6-ipcserver-socketbinder -a 0600 ''${xdg-runtime-dir}/gnupg/S.gpg-agent.ssh
        fdmove 4 0

        export XDG_RUNTIME_DIR ''${xdg-runtime-dir}
        export LISTEN_FDS 2
        export LISTEN_FDNAMES std:ssh
        getpid LISTEN_PID

        fdmove -c 2 1
        if { test -d ''${xdg-runtime-dir} }
        ${pkgs.gnupg}/bin/gpg-agent --supervised
      '';
    };
  };
}
