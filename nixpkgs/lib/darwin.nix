{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isDarwin;

  dstDir = "${config.home.homeDirectory}/Applications/Home Manager Apps";

  apps = pkgs.buildEnv {
    name = "home-manager-applications";
    paths = config.home.packages;
    pathsToLink = "/Applications";
  };
in
{
  home.activation = mkIf isDarwin {
    copyApplications = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      copyApplications() {
        if [[ -d "${dstDir}" ]]; then
          $DRY_RUN_CMD rm -rf $VERBOSE_ARG "${dstDir}"
        fi
        $DRY_RUN_CMD mkdir -p $VERBOSE_ARG "${dstDir}"
        for app in "${apps}/Applications/"*.app; do
          if [[ ! -d "$app" ]]; then
            continue
          fi
          dest="${dstDir}/$(basename "$app")"
          $DRY_RUN_CMD cp -fRL $VERBOSE_ARG "$app" "${dstDir}"
          $DRY_RUN_CMD chmod -R $VERBOSE_ARG +w "$dest"
        done
      }

      copyApplications
    '';
  };
}
