{ config, ... }:

let
  inherit (config.home) homeDirectory;
in
{
  flatpak.enable = true;
  machine.gui.enable = true;
  runit.enable = true;

  imports = [
    ../common.nix
    ../common-linux.nix

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
