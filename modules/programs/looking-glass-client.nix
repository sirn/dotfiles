{ config, lib, pkgs, ... }:

let
  cfg = config.programs.looking-glass-client;
  settingsFormat = pkgs.formats.ini { };
in
{
  programs.looking-glass-client = {
    # Looking Glass requires EGL, which doesn't work when versions between
    # Nix and the host mismatched. Only enable on NixOS.
    enable = config.machine.isNixOS;

    package = pkgs.looking-glass-client;

    settings = {
      input = {
        escapeKey = 100;
        grabKeyboardOnFocus = true;
        captureOnly = true;
        autoCapture = true;
        rawMouse = true;
      };

      spice = {
        enable = true;
        audio = true;
        captureOnStart = true;
      };

      egl = {
        scale = 2;
      };
    };
  };

  # Configure-only when included
  xdg = lib.mkIf (!config.programs.looking-glass-client.enable) {
    configFile = {
      "looking-glass/client.ini" = lib.mkIf (cfg.settings != { }) {
        source = settingsFormat.generate ("looking-glass-client.ini") cfg.settings;
      };
    };
  };
}
