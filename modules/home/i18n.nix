{ config, lib, pkgs, ... }:

{
  # On non-NixOS, this should be installed using OS package manager.
  i18n.inputMethod = lib.mkIf config.machine.isNixOS {
    enable = true;
    type = "fcitx5";

    fcitx5 = {
      waylandFrontend = true;
      addons = with pkgs; [
        fcitx5-mozc
      ];
    };
  };
}
