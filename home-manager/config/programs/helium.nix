{
  lib,
  config,
  pkgs,
  ...
}:

{
  # Helium is a Chromium-based browser shipped as a prebuilt Linux binary
  # (see pkgs/by-name/helium). There is no upstream Home Manager module,
  # so install it as a user package. On non-NixOS Linux, wrap with NixGL so
  # OpenGL/Vulkan libraries are available; on NixOS the wrapper is a
  # passthrough. macOS is not yet supported upstream.
  home.packages = lib.mkIf (pkgs.stdenv.isLinux) [ (config.lib.nixGL.wrap pkgs.local.helium) ];
}
