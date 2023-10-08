{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isLinux;
  inherit (config.home) username homeDirectory;
in
{
  runit.services = {
    xlocate = {
      runScript = ''
        #!${pkgs.execline}/bin/execlineb
        emptyenv -p
        export PATH ${pkgs.gitMinimal}/bin:${pkgs.execline}/bin:${pkgs.busybox}/bin
        export HOME ${homeDirectory}

        fdmove -c 2 1
        foreground { mkdir -p ${homeDirectory}/.local/var/run }
        ${pkgs.snooze}/bin/snooze -v -R 10m -s 6h -H/6 -t ${homeDirectory}/.local/var/run/xlocate_timefile
        if { nice -n 20 /usr/bin/xlocate -S }
        touch ${homeDirectory}/.local/var/run/xlocate_timefile
      '';
    };
  };
}
