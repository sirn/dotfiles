{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
in
mkIf config.machine.gui.enable {
  programs.looking-glass-client = {
    enable = true;

    # Looking Glass requires EGL, which doesn't work when versions between
    # Nix and the host mismatched. Only enable for NixOS. Also,
    # looking-glass-client.package can't use null.
    package =
      if config.machine.nixos.enable
      then pkgs.looking-glass-client
      else
        pkgs.writeTextFile {
          name = "looking-glass-client";
          text = "install looking-glass using os package manager";
          destination = "/var/looking-glass-client";
        };

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
}
