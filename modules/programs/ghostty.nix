{ config, pkgs, ... }:

{
  programs.ghostty = {
    enable = true;

    package =
      if pkgs.stdenv.isDarwin
      # Nix currently marked ghostty (non-bin) as broken on Darwin.
      # TODO: revisit >= 25.11
      then pkgs.unstable.ghostty-bin
      # If NixGL is configured (i.e. non-NixOS), wrap with NixGL
      # so OpenGL/Vulkan libraries are available.
      else config.lib.nixGL.wrap pkgs.unstable.ghostty;

    settings = {
      font-family = "PragmataPro Mono Liga";

      font-size =
        if pkgs.stdenv.isDarwin
        then 14
        else 12;
    };
  };
}
