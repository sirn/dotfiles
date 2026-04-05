{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.services.displayManager.sddm;
in
{
  services.displayManager.sddm = {
    enable = true;
    enableHidpi = true;

    wayland = {
      enable = true;
    };

    settings = {
      Wayland = {
        CompositorCommand = lib.concatStringsSep " " [
          cfg.wayland.compositorCommand
          "--inputmethod ${pkgs.maliit-keyboard}/bin/maliit-keyboard"
        ];
      };
    };
  };
}
