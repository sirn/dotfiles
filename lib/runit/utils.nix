{ config, lib, pkgs, ... }:

with lib;

rec {
  mkRunit =
    { rname
    , serviceDir
    , logDir
    ,
    }:
    let
      inherit (config.home) homeDirectory;

      rprefix =
        if rname == "runit"
        then ""
        else "${rname}-";

      rsuffix =
        if rname == "runit"
        then ""
        else "-${rname}";

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
            description = "A run script for ${rname} log service.";
          };
        };
      };

      runitOpts = { name, config, ... }: {
        options = {
          name = mkOption {
            type = types.str;
            readOnly = true;
            description = ''
              Unique identifier of the ${rname} service. This is set
              to the attribute name of the ${rname} configuration.
            '';
          };

          runScript = mkOption {
            type = types.lines;
            default = "";
            description = "A run script for ${rname} service.";
            example = ''
              #!/bin/sh
              echo hello
              sleep 60
            '';
          };

          finishScript = mkOption {
            type = types.nullOr types.lines;
            default = null;
            description = "A finish script for ${rname} service.";
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
          config."${rname}".services);

      finishScripts =
        (mapAttrs (n: v: v.finishScript)
          (filterAttrs (n: v: v.finishScript != null)
            config."${rname}".services));

      logScripts = (mapAttrs
        (n: v:
          if v.log.runScript != null then v.log.runScript else ''
            #!${pkgs.execline}/bin/execlineb
            emptyenv -p
            export PATH ${pkgs.execline}/bin:${pkgs.busybox}/bin
            if { mkdir -p ${homeDirectory}/${logDir}/${n} }
            ${pkgs.s6}/bin/s6-log
                -b n10 s1000000 t
                !"${pkgs.gzip}/bin/gzip -nq9"
                ${homeDirectory}/${logDir}/${rprefix}${n}
          '')
        (filterAttrs (n: v: v.log.enable) config."${rname}".services));

      mkScripts = f: services:
        mapAttrs'
          (n: v: nameValuePair (f n) { executable = true; text = v; })
          services;
    in
    {
      options = {
        "${rname}" = {
          enable = mkOption {
            type = types.bool;
            description = "Whether to enable ${rname}.";
            default = false;
          };

          serviceDir = mkOption {
            type = types.str;
            description = "Path to install ${rname} service.";
            default = serviceDir;
          };

          logDir = mkOption {
            type = types.str;
            description = "Path to store ${rname} logs.";
            default = serviceDir;
          };

          services = mkOption {
            type = types.attrsOf (types.submodule runitOpts);
            default = { };
            description = "List of ${rname} services.";
          };
        };
      };

      config =
        let
          serviceDir = config."${rname}".serviceDir;
          logDir = config."${rname}".logDir;
        in
        {
          home.file = mkMerge [
            (mkScripts (n: "${serviceDir}/${n}/run") runScripts)
            (mkScripts (n: "${serviceDir}/${n}/finish") finishScripts)
            (mkScripts (n: "${serviceDir}/${n}/log/run") logScripts)
          ];

          # NOTE: runit supervise directory can't be symlinked from the Nix store
          # since runit only readlink for one level to create run directory
          home.activation = mkIf config."${rname}".enable {
            "setupRunitServices${rsuffix}" = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
              setupRunitServices${rsuffix}() {
                local svcs
                declare -A svcs
                svcs=(${
                  concatStringsSep " " (
                    (mapAttrsToList
                      (n: v: "['${n}']=${homeDirectory}/${serviceDir}/${n}")
                      runScripts) ++
                    (mapAttrsToList
                      (n: v: "['${n}-log']=${homeDirectory}/${serviceDir}/${n}/log")
                      logScripts)
                  )})
                for svc in ''${!svcs[@]}; do
                  src="/run/runit.$USER/supervise.${rprefix}$svc"
                  dest="''${svcs[$svc]}/supervise"
                  $DRY_RUN_CMD ln -nsf $VERBOSE_ARG "$src" "$dest"
                done

                # Cleanup old services; logs are cleaned first
                if [[ -n $oldGenPath ]]; then
                  oldSvcPath=$oldGenPath/home-files/${serviceDir}
                  newSvcPath=$newGenPath/home-files/${serviceDir}

                  for svc in "$oldSvcPath"/*/log "$oldSvcPath"/*; do
                    svcName=''${svc##$oldSvcPath/}
                    if [[ -d "$svc" ]] && [[ ! -e "$newSvcPath/$svcName" ]]; then
                      $DRY_RUN_CMD rm \
                        $VERBOSE_ARG \
                        "${homeDirectory}/${serviceDir}/$svcName/supervise"
                      $DRY_RUN_CMD rmdir -p --ignore-fail-on-non-empty \
                        $VERBOSE_ARG \
                        "${homeDirectory}/${serviceDir}/$svcName"
                    fi
                  done
                fi
              }

              setupRunitServices${rsuffix}
            '';
          };
        };
    };
}
