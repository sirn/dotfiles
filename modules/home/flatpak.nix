{ config, lib, pkgs, ... }:

let
  fontConfig = pkgs.writeTextFile {
    name = "fonts.conf";
    text = ''
      <?xml version="1.0"?>
      <!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
      <fontconfig>
        <description>Load per-user customization files</description>
        <include ignore_missing="yes" prefix="xdg">fontconfig/conf.d</include>
        <include ignore_missing="yes" prefix="xdg">fontconfig/fonts.conf</include>
      </fontconfig>
    '';
  };
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
