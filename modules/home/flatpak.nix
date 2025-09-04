{ config, lib, pkgs, ... }:

{
  flatpak = {
    enable = true;

    globalOverrides = {
      filesystems = [
        "/nix/store:ro"
        "/usr/share/icons:ro"
        "xdg-config/fontconfig:ro"
        "~/.local/share/fonts:ro"
        "~/.local/share/icons:ro"
        "~/.nix-profile/share/icons:ro"
        "~/.nix-profile/share/fonts:ro"
        "/nix/store:ro" # :(
      ];

      environment = {
        XCURSOR_PATH = lib.concatStringsSep ":" [
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
