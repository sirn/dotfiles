{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isLinux;
  inherit (config.home) username;

  homeDir = config.home.homeDirectory;
in
{
  home = {
    file = {
      ".local/var/service/gpg-agent/run" = {
        executable = true;
        text = ''
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

          export HOME ${homeDir}
          export XDG_RUNTIME_DIR ''${xdg-runtime-dir}
          export LISTEN_FDS 2
          export LISTEN_FDNAMES std:ssh
          getpid LISTEN_PID

          if {
              test -d ''${xdg-runtime-dir}
          }

          fdmove -c 2 1
          ${pkgs.gnupg}/bin/gpg-agent --supervised
        '';
      };
      ".local/var/service/gpg-agent/log/run" = {
        executable = true;
        text = ''
          #!${pkgs.execline}/bin/execlineb

          emptyenv -p
          export PATH ${pkgs.execline}/bin:${pkgs.busybox}/bin
          define logpath ${homeDir}/.local/var/log/gpg-agent

          if { mkdir -p ''${logpath} }
          ${pkgs.s6}/bin/s6-log -b n10 s1000000 t !"${pkgs.gzip}/bin/gzip -nq9" ''${logpath}
        '';
      };
    };

    # Runit only readlink for one level; mkOutOfStoreSymlink creates nested
    # symlinks of src->/nix/store->/nix/store/hm_...->dest which confuse runit
    activation = {
      gpgAgentSuperviseActivation = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -p $VERBOSE_ARG "$HOME/.local/var/service/gpg-agent/log"
        $DRY_RUN_CMD ln -sf $VERBOSE_ARG "/run/runit.${username}/supervise.gpg-agent" "$HOME/.local/var/service/gpg-agent/supervise"
        $DRY_RUN_CMD ln -sf $VERBOSE_ARG "/run/runit.${username}/supervise.gpg-agent-log" "$HOME/.local/var/service/gpg-agent/log/supervise"
      '';
    };
  };
}
