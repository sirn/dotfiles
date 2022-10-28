{ config, lib, pkgs, ... }:

with lib;
let
  dstDir = "${config.home.homeDirectory}/.local/var/service";
  logDir = "${config.home.homeDirectory}/.local/var/log";

  logModule = types.submodule {
    options = {
      enable = mkOption {
        type = types.bool;
        description = "Whether to enable logging.";
        default = true;
      };

      runScript = mkOption {
        type = types.nullOr types.lines;
        default = null;
        description = "A run script for runit log service.";
      };
    };
  };

  runitOpts = { name, config, ... }: {
    options = {
      name = mkOption {
        type = types.str;
        readOnly = true;
        description = ''
          Unique identifier of the runit service. This is set
          to the attribute name of the runit configuration.
        '';
      };

      runScript = mkOption {
        type = types.lines;
        default = "";
        description = "A run script for runit service.";
        example = ''
          #!/bin/sh
          echo hello
          sleep 60
        '';
      };

      finishScript = mkOption {
        type = types.nullOr types.lines;
        default = null;
        description = "A finish script for runit service.";
        example = ''
          #!/bin/sh
          exit 0
        '';
      };

      log = mkOption {
        type = logModule;
        description = "Logging configuration for service.";
        default = { };
      };

      config = {
        name = name;
      };
    };
  };

  runScripts =
    (mapAttrs (n: v: v.runScript)
      config.runit.services);

  finishScripts =
    (mapAttrs (n: v: v.finishScript)
      (filterAttrs (n: v: v.finishScript != null) config.runit.services));

  logScripts = (mapAttrs
    (n: v:
      if v.log.runScript != null then v.log.runScript else ''
        #!${pkgs.execline}/bin/execlineb
        emptyenv -p
        export PATH ${pkgs.execline}/bin:${pkgs.busybox}/bin
        if { mkdir -p ${logDir}/${n} }
        ${pkgs.s6}/bin/s6-log
            -b n10 s1000000 t
            !"${pkgs.gzip}/bin/gzip -nq9"
            ${logDir}/${n}
      '')
    (filterAttrs (n: v: v.log.enable) config.runit.services));

  mkScripts = f: services:
    mapAttrs'
      (n: v: nameValuePair (f n) { executable = true; text = v; })
      services;
in
{
  options = {
    runit = {
      services = mkOption {
        type = types.attrsOf (types.submodule runitOpts);
        default = { };
        description = "List of runit services.";
      };
    };
  };

  config = {
    home.file = mkMerge [
      (mkScripts (n: "${dstDir}/${n}/run") runScripts)
      (mkScripts (n: "${dstDir}/${n}/finish") finishScripts)
      (mkScripts (n: "${dstDir}/${n}/log/run") logScripts)
    ];

    # NOTE: runit supervise directory can't be symlinked from the Nix store
    # since runit only readlink for one level to create run directory
    home.activation.setupRunitServices = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      setupRunitServices() {
        local svcs
        declare -A svcs
        svcs=(${
          concatStringsSep " " (
            (mapAttrsToList (n: v: "['${n}']=${dstDir}/${n}") runScripts) ++
            (mapAttrsToList (n: v: "['${n}-log']=${dstDir}/${n}/log") logScripts)
          )})
        for svc in ''${!svcs[@]}; do
          src="/run/runit.$USER/supervise.$svc"
          dest="''${svcs[$svc]}/supervise"
          $DRY_RUN_CMD ln -nsf $VERBOSE_ARG "$src" "$dest"
        done
      }

      setupRunitServices
    '';
  };
}
