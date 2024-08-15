{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isLinux;
  inherit (lib) mkDefault mkIf;

  cfg = config.programs.looking-glass-client;
  settingsFormat = pkgs.formats.ini { };
in
{
  programs.looking-glass-client = {
    # Looking Glass requires EGL, which doesn't work when versions between
    # Nix and the host mismatched. Only enable on NixOS.
    enable = config.machine.isNixOS;

    package = mkDefault pkgs.local.looking-glass-client_b6;

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
  xdg = mkIf (!config.programs.looking-glass-client.enable) {
    configFile = {
      "looking-glass/client.ini" = mkIf (cfg.settings != { }) {
        source = settingsFormat.generate ("looking-glass-client.ini") cfg.settings;
      };
    };
  };
}
