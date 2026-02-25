{ config, lib, pkgs, ... }:

let
  cfg = config.programs.looking-glass-client;
in
{
  programs.looking-glass-client = {
    enable = true;

    # If NixGL is configured (i.e. non-NixOS), wrap with NixGL
    # so OpenGL/Vulkan libraries are available.
    package = config.lib.nixGL.wrap pkgs.looking-glass-client;

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
}
