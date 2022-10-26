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
      ".local/var/service/xlocate/run" = {
        executable = true;
        text = ''
          #!/nix/var/nix/profiles/default/bin/nix-shell
          #!nix-shell -i execlineb -p execline snooze

          emptyenv -p

          export HOME ${homeDir}

          fdmove -c 2 1
          foreground { mkdir -p ${homeDir}/.local/var/run }
          snooze -v -R 10m -s 6h -H/6 -t ${homeDir}/.local/var/run/xlocate_timefile
          if { nice -n 20 xlocate -S }
          touch ${homeDir}/.local/var/run/xlocate_timefile
        '';
      };
      ".local/var/service/xlocate/log/run" = {
        executable = true;
        text = ''
          #!/nix/var/nix/profiles/default/bin/nix-shell
          #!nix-shell -i execlineb -p execline s6 gzip

          emptyenv -p

          define logpath ${homeDir}/.local/var/log/xlocate

          if { mkdir -p ''${logpath} }
          s6-log -b n10 s1000000 t !"gzip -nq9" ''${logpath}
        '';
      };
    };

    # Runit only readlink for one level; mkOutOfStoreSymlink creates nested
    # symlinks of src->/nix/store->/nix/store/hm_...->dest which confuse runit
    activation = {
      xlocateSuperviseActivation = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -p $VERBOSE_ARG "$HOME/.local/var/service/xlocate/log"
        $DRY_RUN_CMD ln -sf $VERBOSE_ARG "/run/runit.${username}/supervise.xlocate" "$HOME/.local/var/service/xlocate/supervise"
        $DRY_RUN_CMD ln -sf $VERBOSE_ARG "/run/runit.${username}/supervise.xlocate-log" "$HOME/.local/var/service/xlocate/log/supervise"
      '';
    };
  };
}
