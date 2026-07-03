{
  lib,
  config,
  pkgs,
  ...
}:

{
  programs.vivaldi = {
    enable = true;

    # If NixGL is configured (i.e. non-NixOS), wrap with NixGL so OpenGL/Vulkan libraries are available.
    # On macOS, user should install Vivaldi by themselves.
    package = lib.mkDefault (if pkgs.stdenv.isLinux then config.lib.nixGL.wrap pkgs.vivaldi else null);
  };
}
