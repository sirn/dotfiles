{ lib, pkgs }:

pkgs.xdg-desktop-portal-kde.overrideAttrs (orig: rec {
  buildInputs = with pkgs; orig.buildInputs ++ [
    breeze-icons
    ffmpegthumbnailer
    libsForQt5.kdegraphics-thumbnailers
    libsForQt5.kio-extras
  ];
})
