{ config, lib, pkgs, ... }:

with lib;
let
  overrideModule = types.submodule {
    options = {
      filesystems = mkOption {
        type = with types; listOf str;
        default = [ ];
        description = ''
          List of filesystem paths to allow in an application.
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
    if v.filesystems != [ ]
    then [
      (nameValuePair "Context" {
        filesystems = v.filesystems;
      })
    ]
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

  config = {
    xdg.dataFile = listToAttrs (
      (if config.flatpak.globalOverrides != null
      then [ (mkOverrideFile "global" config.flatpak.globalOverrides) ]
      else [ ]) ++
      (map
        (n: mkOverrideFile n config.flatpak.applications.${n}.overrides)
        (attrNames config.flatpak.applications))
    );

    home.activation = mkIf config.flatpak.enable {
      # TODO: have Home Manager manage Flatpak installations?
      noticeFlatpakApplications = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        noticeFlatpakApplications() {
          appids=(${concatStringsSep " " (mapAttrsToList (n: v: v.name) config.flatpak.applications)})
          if [[ ''${#appids[@]} -lt 1 ]]; then
            return
          fi

          echo "Flatpak applications are not managed by Home Manager."
          echo "The following applications should be installed manually:"
          for appid in $appids; do
            echo "  $appid"
          done
        }

        noticeFlatpakApplications
      '';
    };
  };
}
