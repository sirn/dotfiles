{ config, lib, pkgs, ... }:

with lib; {
  options = {
    machine = {
      gui = {
        enable = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Whether to enable GUI support. Unlike setting gtk.enable and/or qt.enable
            this option does not write any settings and intended solely to be consumed
            by other modules.
          '';
        };
      };

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
          type = with types; listOf str;
          default = [ ];
          description = ''
            List of Flatpak application IDs to notice the user to install.
          '';
        };
      };
    };
  };

  config = {
    home.activation = mkIf config.machine.flatpak.enable {
      # TODO: have Home Manager manage Flatpak installations?
      noticeFlatpakApplications = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        noticeFlatpakApplications() {
          appids=(${concatStringsSep " " (config.machine.flatpak.applications)})
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
