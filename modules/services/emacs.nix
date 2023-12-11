{ config, lib, pkgs, ... }:

let
  inherit (config.home) username homeDirectory;
  inherit (pkgs.stdenv) isLinux isDarwin;
  inherit (lib) mkIf;
in
{
  services = mkIf config.machine.isNixOS {
    emacs = {
      enable = true;
      socketActivation = {
        enable = true;
      };
    };
  };

  runit.services = mkIf (isLinux && config.runit.enable) {
    emacs = {
      runScript = ''
        #!${pkgs.execline}/bin/execlineb
        emptyenv -p
        export PATH ${pkgs.execline}/bin:${pkgs.busybox}/bin
        export HOME ${homeDirectory}

        backtick -n -E uid { id -u }
        define xdg-runtime-dir ${config.machine.xdgRuntimePrefix}/''${uid}
        if { test -d ''${xdg-runtime-dir} }

        backtick -n -E user { id -un }
        backtick -n -E shell {
            redirfd -r 0 /etc/passwd
            awk "BEGIN { FS=\":\" } /^''${user}:/ { print $7 }"
        }

        export USER ''${user}
        export SHELL ''${shell}
        export XDG_RUNTIME_DIR ''${xdg-runtime-dir}

        backtick -n -E agent-ssh-socket { ${pkgs.gnupg}/bin/gpgconf --list-dirs agent-ssh-socket }
        export SSH_AUTH_SOCK ''${agent-ssh-socket}

        fdmove -c 2 1
        ''${shell} -l -c "${config.programs.emacs.finalPackage}/bin/emacs --fg-daemon --chdir=${homeDirectory}"
      '';
    };
  };

  launchd.agents.emacs = mkIf isDarwin {
    enable = true;
    config = {
      RunAtLoad = true;
      KeepAlive = true;
      ProgramArguments = [
        "/bin/sh"
        "-l"
        "-c"
        "${config.programs.emacs.finalPackage}/bin/emacs --fg-daemon --chdir=$HOME"
      ];
    };
  };
}
