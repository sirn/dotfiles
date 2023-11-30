{ pkgs, config, ... }:

let
  inherit (config.home) homeDirectory;
in
{
  desktop.enable = true;
  flatpak.enable = true;
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

  # Nvidia can be pretty problematic.
  # See also https://github.com/alacritty/alacritty/issues/6359
  home.sessionVariablesExtra = ''
    export __EGL_VENDOR_LIBRARY_FILENAMES=/usr/share/glvnd/egl_vendor.d/50_mesa.json
  '';
}
