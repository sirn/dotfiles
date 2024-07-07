{ config, lib, pkgs, ... }:

let
  inherit (lib)
    attrNames concatStringsSep generators isList listToAttrs mapAttrsToList
    mkIf mkMerge mkOption nameValuePair types;

  overrideModule = types.submodule {
    options = {
      filesystems = mkOption {
        type = with types; listOf str;
        default = [ ];
        description = ''
          List of filesystem paths to allow in an application.
        '';
      };

      sockets = mkOption {
        type = with types; listOf str;
        default = [ ];
        description = ''
          List of extra sockets to allow in an application.
        '';
      };

      talk-names = mkOption {
        type = with types; listOf str;
        default = [ ];
        description = ''
          List of talk names to allow in an application.
        '';
      };

      environment = mkOption {
        type = with types; attrsOf str;
        default = { };
        description = ''
          Extra environment variable to set in Flatpak application.
        '';
      };
    };
  };

  flatpakOpts = { name, config, ... }: {
    options = {
      name = mkOption {
        type = types.str;
        readOnly = true;
        description = ''
          Unique application ID for Flatpak applications.
          This is set to the attribute name of the Flatpak
          application.
        '';
      };

      overrides = mkOption {
        type = overrideModule;
        description = "Override configuration for Flatpak applications.";
        default = { };
      };
    };

    config = {
      name = name;
    };
  };

  renderOptionValue = v:
    if isList v then concatStringsSep ";" v
    else generators.mkValueStringDefault { } v;

  renderOptions = generators.toINI {
    mkKeyValue = generators.mkKeyValueDefault { mkValueString = renderOptionValue; } "=";
    listsAsDuplicateKeys = false;
  };

  renderOverrideContextSection = v:
    let
      contextAttrs = (listToAttrs ([ ] ++
        (if v.filesystems != [ ] then [ (nameValuePair "filesystems" v.filesystems) ] else [ ]) ++
        (if v.sockets != [ ] then [ (nameValuePair "sockets" v.sockets) ] else [ ]) ++
        (if v.talk-names != [ ] then [ (nameValuePair "talk-names" v.talk-names) ] else [ ])
      ));
    in
    if contextAttrs != { }
    then [ (nameValuePair "Context" contextAttrs) ]
    else [ ];

  renderOverrideEnvironmentSection = v:
    if v.environment != { }
    then [ (nameValuePair "Environment" v.environment) ]
    else [ ];

  mkOverrideFile = n: v: nameValuePair "flatpak/overrides/${n}" {
    text = renderOptions (listToAttrs (
      (renderOverrideContextSection v) ++
      (renderOverrideEnvironmentSection v)
    ));

    # Flatpak has tendency to override symlinks with generic file whenever
    # `flatpak override' command was called. We want nix-controlled configuration
    # to be the source of truth. So uh...
    force = true;
  };
in
{
  options = {
    flatpak = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Flatpak support. This option does not write any settings
          and intended solely to be consumed by other modules.
        '';
      };

      applications = mkOption {
        type = types.attrsOf (types.submodule flatpakOpts);
        default = { };
        description = "List of Flatpak application settings.";
      };

      globalOverrides = mkOption {
        type = types.nullOr overrideModule;
        default = null;
        description = "Define global overrides";
      };
    };
  };

  config = mkIf config.flatpak.enable {
    xdg.dataFile = listToAttrs (
      (if config.flatpak.globalOverrides != null
      then [ (mkOverrideFile "global" config.flatpak.globalOverrides) ]
      else [ ]) ++
      (map
        (n: mkOverrideFile n config.flatpak.applications.${n}.overrides)
        (attrNames config.flatpak.applications))
    );

    home.activation =
      let
        inherit (config.home) homeDirectory;
      in
      {
        # TODO: have Home Manager manage Flatpak installations?
        noticeFlatpakApplications = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          noticeFlatpakApplications() {
            appids=(${concatStringsSep " " (mapAttrsToList (n: v: v.name) config.flatpak.applications)})
            if [[ ''${#appids[@]} -lt 1 ]]; then
              return
            fi

            echo "Flatpak applications are not managed by Home Manager."
            echo "Suggested commands:"
            for appid in ''${appids[@]}; do
              echo "flatpak install $appid"
            done
          }

          noticeFlatpakApplications
        '';

        cleanFlatpakDatadir = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          cleanFlatpakDatadir() {
            baseDir="${homeDirectory}/.var/app"

            for appDir in "''${baseDir}"/*; do
              if [ ! -d "''${appDir}" ]; then
                continue
              fi

              appName=''${appDir##''$baseDir/}
              nixProfileDir=''${appDir}/.local/state/nix

              if [ -d "''${nixProfileDir}/profiles" ]; then
                echo "Cleaning up $appName"
                rm "''${nixProfileDir}/profiles"/*
                rm -d "''${nixProfileDir}/profiles"
                rm -d "''${nixProfileDir}"
              fi
            done
          }

          cleanFlatpakDatadir
        '';
      };
  };
}
