{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isLinux;
  inherit (config.home) username;

  homeDir = config.home.homeDirectory;
in
{
  home = mkIf isLinux {
    file = {
      ".local/var/service/gpg-agent/run" = {
        executable = true;
        text = ''
          #!/nix/var/nix/profiles/default/bin/nix-shell
          #!nix-shell -i execlineb -p execline s6 gnupg

          emptyenv -p

          backtick -n -E uid { id -u }
          define xdg-runtime-dir /run/user/''${uid}

          s6-ipcserver-socketbinder -a 0600 ''${xdg-runtime-dir}/gnupg/S.gpg-agent
          fdmove 3 0
          s6-ipcserver-socketbinder -a 0600 ''${xdg-runtime-dir}/gnupg/S.gpg-agent.ssh
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
          gpg-agent --supervised
        '';
      };
      ".local/var/service/gpg-agent/log/run" = {
        executable = true;
        text = ''
          #!/nix/var/nix/profiles/default/bin/nix-shell
          #!nix-shell -i execlineb -p execline s6 gzip

          emptyenv -p

          define logpath ${homeDir}/.local/var/log/gpg-agent

          if { mkdir -p ''${logpath} }
          s6-log -b n10 s1000000 t !"gzip -nq9" ''${logpath}
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
