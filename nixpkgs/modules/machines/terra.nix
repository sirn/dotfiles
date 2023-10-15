{ config, ... }:

let
  inherit (config.home) homeDirectory;
in
{
  machine.gui.enable = true;

  imports = [
    ../common.nix

    # profile
    ../home/flatpak.nix
    ../home/fonts.nix
    ../home/runit.nix

    # programs
    ../programs/emacsc.nix
    ../programs/firefox.nix
    ../programs/foot.nix
    ../programs/fuzzel.nix
    ../programs/intellij.nix
    ../programs/kanshi.nix
    ../programs/mpv.nix
    ../programs/obsidian.nix
    ../programs/sway.nix
    ../programs/thunderbird.nix
    ../programs/waybar.nix

    # services
    ../runit/emacs.nix
    ../runit/gpg-agent.nix
    ../runit/syncthing.nix
    ../runit/xlocate.nix
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
