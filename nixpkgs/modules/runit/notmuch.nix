{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isLinux;
  inherit (config.home) username homeDirectory;
in
{
  runit.services = {
    notmuch = {
      runScript = ''
        #!${pkgs.execline}/bin/execlineb
        emptyenv -p
        export PATH ${pkgs.execline}/bin:${pkgs.busybox}/bin
        export HOME ${homeDirectory}

        fdmove -c 2 1
        foreground { mkdir -p ${homeDirectory}/.local/var/run }
        ${pkgs.snooze}/bin/snooze -v -R 30 -s 1m -H/1 -M/1 -t ${homeDirectory}/.local/var/run/notmuch_timefile
        if { nice -n 20 ${pkgs.notmuch}/bin/notmuch new }
        touch ${homeDirectory}/.local/var/run/notmuch_timefile
      '';
    };
  };
}
