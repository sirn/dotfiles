{ config, lib, pkgs, ... }:

let
  inherit (pkgs.stdenv) isLinux;
  inherit (lib) mkIf;

  cfg = config.programs.looking-glass-client;
  settingsFormat = pkgs.formats.ini { };
in
mkIf (isLinux && config.desktop.enable) {
  programs.looking-glass-client = {
    # Looking Glass requires EGL, which doesn't work when versions between
    # Nix and the host mismatched. Only enable on NixOS.
    enable = config.machine.isNixOS;

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

      audio = {
        periodSize = 4096;
      };
    };
  };

  # Configure only in case of non-NixOS
  xdg = mkIf (!config.machine.isNixOS) {
    configFile = {
      "looking-glass/client.ini" = mkIf (cfg.settings != { }) {
        source = settingsFormat.generate ("looking-glass-client.ini") cfg.settings;
      };
    };
  };
}
