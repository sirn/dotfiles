{ config, ... }:

let
  inherit (config.home) homeDirectory;
in
{
  machine.runit.enable = true;
  machine.gui.enable = true;

  imports = [
    ../common.nix
    ../common-linux-desktop.nix

    # services
    ../services/syncthing.nix
    ../services/xlocate.nix
  ];

  wayland.windowManager.sway = {
    config = {
      output = {
        "*" = {
          bg = "${homeDirectory}/Pictures/Wallpapers/Manifest/Moonrise.jpg fill";
        };
      };
    };
  };
}
