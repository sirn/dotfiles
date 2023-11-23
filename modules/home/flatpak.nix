{ config, lib, ... }:

{
  flatpak = {
    enable = true;

    globalOverrides = {
      filesystems = [
        "/nix/store:ro"
        "/usr/share/icons:ro"
        "xdg-config/fontconfig:ro"
        "xdg-config/gtk-3.0:ro"
        "~/.dotfiles/etc/fontconfig:ro"
        "~/.local/share/fonts:ro"
        "~/.local/share/icons:ro"
      ];

      environment = {
        FONTCONFIG_FILE = "${config.home.homeDirectory}/.config/fontconfig/fonts.conf";
      };
    };
  };
}
