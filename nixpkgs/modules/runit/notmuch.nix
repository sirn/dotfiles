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
      ".local/var/service/notmuch/run" = {
        executable = true;
        text = ''
          #!${pkgs.execline}/bin/execlineb

          emptyenv -p
          export PATH ${pkgs.execline}/bin:${pkgs.busybox}/bin
          export HOME ${homeDir}

          fdmove -c 2 1
          foreground { mkdir -p ${homeDir}/.local/var/run }
          ${pkgs.snooze}/bin/snooze -v -R 30 -s 1m -H/1 -M/1 -t ${homeDir}/.local/var/run/notmuch_timefile
          if { nice -n 20 ${pkgs.notmuch}/bin/notmuch new }
          touch ${homeDir}/.local/var/run/notmuch_timefile
        '';
      };
      ".local/var/service/notmuch/log/run" = {
        executable = true;
        text = ''
          #!${pkgs.execline}/bin/execlineb

          emptyenv -p
          export PATH ${pkgs.execline}/bin:${pkgs.busybox}/bin
          define logpath ${homeDir}/.local/var/log/notmuch

          if { mkdir -p ''${logpath} }
          ${pkgs.s6}/bin/s6-log -b n10 s1000000 t !"${pkgs.gzip}/bin/gzip -nq9" ''${logpath}
        '';
      };
    };

    # Runit only readlink for one level; mkOutOfStoreSymlink creates nested
    # symlinks of src->/nix/store->/nix/store/hm_...->dest which confuse runit
    activation = {
      notmuchSuperviseActivation = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -p $VERBOSE_ARG "$HOME/.local/var/service/notmuch/log"
        $DRY_RUN_CMD ln -sf $VERBOSE_ARG "/run/runit.${username}/supervise.notmuch" "$HOME/.local/var/service/notmuch/supervise"
        $DRY_RUN_CMD ln -sf $VERBOSE_ARG "/run/runit.${username}/supervise.notmuch-log" "$HOME/.local/var/service/notmuch/log/supervise"
      '';
    };
  };
}
