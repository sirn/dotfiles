{ config, lib, pkgs, ... }:

let
  inherit (config.home) username homeDirectory;
in
{
  runit.services = {
    emacs = {
      runScript = ''
        #!${pkgs.execline}/bin/execlineb
        emptyenv -p
        export PATH ${pkgs.execline}/bin:${pkgs.busybox}/bin
        export HOME ${homeDirectory}

        backtick -n -E uid { id -u }
        backtick -n -E user { id -un }
        define xdg-runtime-dir /run/user/''${uid}
        backtick -n -E shell {
            redirfd -r 0 /etc/passwd
            awk "BEGIN { FS=\":\" } /^''${user}:/ { print $7 }"
        }

        export USER ''${user}
        export SHELL ''${shell}
        export XDG_RUNTIME_DIR ''${xdg-runtime-dir}
        export SSH_AUTH_SOCK ''${xdg-runtime-dir}/gnupg/S.gpg-agent.ssh

        fdmove -c 2 1
        if { test -d ''${xdg-runtime-dir} }
        ''${shell} -l -c "${config.programs.emacs.finalPackage}/bin/emacs --fg-daemon --chdir=${homeDirectory}"
      '';
    };
  };
}
