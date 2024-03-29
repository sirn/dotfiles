{ config, lib, pkgs, ... }:

let
  inherit (config.home) homeDirectory;
  inherit (pkgs.stdenv) isLinux;
  inherit (lib) mkIf;
in
{
  runit.services = mkIf (isLinux && config.runit.enable) {
    xlocate = {
      runScript = ''
        #!${pkgs.execline}/bin/execlineb
        emptyenv -p
        export PATH ${pkgs.gitMinimal}/bin:${pkgs.execline}/bin:${pkgs.busybox}/bin
        export HOME ${homeDirectory}

        fdmove -c 2 1
        foreground { mkdir -p ${homeDirectory}/.local/var/run }
        ${pkgs.snooze}/bin/snooze -v -R 10m -s 6h -H/6 -t ${homeDirectory}/.local/var/run/xlocate_timefile
        if { nice -n 20 /usr/sbin/xlocate -S }
        touch ${homeDirectory}/.local/var/run/xlocate_timefile
      '';
    };
  };
}
