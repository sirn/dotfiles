{ config, lib, pkgs, ... }:

{
  programs.mpv = {
    enable = true;

    # If NixGL is configured (i.e. non-NixOS), wrap with NixGL
    # so OpenGL/Vulkan libraries are available.
    package = config.lib.nixGL.wrap pkgs.mpv;

    profiles = lib.mkIf pkgs.stdenv.isLinux {
      hdr = {
        profile-cond = "get(\"video-params/primaries\") == \"bt.2020\"";
        target-colorspace-hint = true;
        target-contrast = "inf";
      };
    };

    config = lib.mkMerge [
      {
        hwdec = "auto";
      }
      (lib.mkIf pkgs.stdenv.isLinux {
        vo = "gpu-next";
        gpu-api = "vulkan";
      })
    ];
  };
}
