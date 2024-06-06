{ config, lib, ... }:

let
  inherit (lib) concatStringsSep mkIf;
in
{
  flatpak = {
    globalOverrides = {
      filesystems = [
        "/nix/store:ro"
        "/usr/share/icons:ro"
        "xdg-config/fontconfig:ro"
        "xdg-config/gtk-3.0:ro"
        "~/.dotfiles/etc/fontconfig:ro"
        "~/.local/share/fonts:ro"
        "~/.local/share/icons:ro"
        "~/.nix-profile/share/icons:ro"
      ];

      environment = {
        FONTCONFIG_FILE = "${config.home.homeDirectory}/.config/fontconfig/fonts.conf";
        XCURSOR_PATH = concatStringsSep ":" [
          "${config.home.homeDirectory}/.local/share/icons"
          "${config.home.homeDirectory}/.nix-profile/share/icons"
          "/nix/profile/share/icons"
          "/nix/var/nix/profiles/default/share/icons"
          "/run/host/share/icons"
          "/usr/share/icons"
        ];
      };
    };
  };
}
