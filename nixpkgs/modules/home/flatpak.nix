{ config, lib, ... }:

let
  inherit (lib) mkIf;
in
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
        "~/.nix-profile/share/fonts:ro"
      ];

      environment = {
        FONTCONFIG_FILE = "${config.home.homeDirectory}/.config/fontconfig/conf.d";
      };
    };
  };
}
