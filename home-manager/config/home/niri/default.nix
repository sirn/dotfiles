{ lib, ... }:

{
  imports = [
    ./base.nix

    ../../programs/fuzzel.nix
    ../../programs/waybar.nix

    ../../services/awww.nix
    ../../services/mako.nix
    ../../services/swayidle.nix
    ../../services/swaylock.nix
    ../../services/wlsunset.nix
  ];

  # We use different geometry settings between bare settings and shell.
  # Since we can't easily redefine window-rules, this is set here instead.
  programs.niri = {
    settings = {
      window-rules = lib.mkBefore [
        {
          clip-to-geometry = true;
          geometry-corner-radius = {
            bottom-left = 4.0;
            bottom-right = 4.0;
            top-left = 4.0;
            top-right = 4.0;
          };
        }
      ];
    };
  };
}
