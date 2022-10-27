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
      ".local/var/service/emacs/run" = {
        executable = true;
        text = ''
          #!${pkgs.execline}/bin/execlineb

          emptyenv -p
          export PATH ${pkgs.execline}/bin:${pkgs.busybox}/bin

          backtick -n -E uid { id -u }
          define xdg-runtime-dir /run/user/''${uid}
          backtick -n -E shell {
              redirfd -r 0 /etc/passwd
              awk "BEGIN { FS=\":\" } /^${username}:/ { print $7 }"
          }

          export HOME ${homeDir}
          export SHELL ''${shell}
          export XDG_RUNTIME_DIR ''${xdg-runtime-dir}
          export SSH_AUTH_SOCK ''${xdg-runtime-dir}/gnupg/S.gpg-agent.ssh

          fdmove -c 2 1
          if { test -d ''${xdg-runtime-dir} }
          ''${shell} -l -c "${config.programs.emacs.package}/bin/emacs --fg-daemon --chdir=${homeDir}"
        '';
      };
      ".local/var/service/emacs/log/run" = {
        executable = true;
        text = ''
          #!${pkgs.execline}/bin/execlineb

          emptyenv -p
          export PATH ${pkgs.execline}/bin:${pkgs.busybox}/bin
          define logpath ${homeDir}/.local/var/log/emacs

          if { mkdir -p ''${logpath} }
          ${pkgs.s6}/bin/s6-log -b n10 s1000000 t !"${pkgs.gzip}/bin/gzip -nq9" ''${logpath}
        '';
      };
    };

    # Runit only readlink for one level; mkOutOfStoreSymlink creates nested
    # symlinks of src->/nix/store->/nix/store/hm_...->dest which confuse runit
    activation = {
      emacsSuperviseActivation = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        $DRY_RUN_CMD mkdir -p $VERBOSE_ARG "$HOME/.local/var/service/emacs/log"
        $DRY_RUN_CMD ln -sf $VERBOSE_ARG "/run/runit.${username}/supervise.emacs" "$HOME/.local/var/service/emacs/supervise"
        $DRY_RUN_CMD ln -sf $VERBOSE_ARG "/run/runit.${username}/supervise.emacs-log" "$HOME/.local/var/service/emacs/log/supervise"
      '';
    };
  };
}
