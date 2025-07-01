{ config, lib, pkgs, ... }:

let
  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
in
{
  fonts = {
    fontconfig = {
      enable = true;
    };
  };

  home.packages = with pkgs; [
    fira
    fira-code
    fira-code-symbols
    fira-mono
    hack-font
    ibm-plex
    inter
    iosevka
    ipaexfont
    ipafont
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    noto-fonts-color-emoji
    noto-fonts-extra
    source-code-pro
    source-han-code-jp
    source-han-mono
    source-han-sans
    source-han-serif
    source-sans-pro
    source-serif-pro
    tlwg
    ubuntu_font_family

    local.ia-writer-duo-static
    local.ia-writer-mono-static
    local.ia-writer-quattro-static
  ];

  # https://wiki.archlinux.org/title/Font_configuration
  gtk.gtk4.extraConfig = lib.mkIf pkgs.stdenv.isLinux {
    gtk-hint-font-metrics = true;
  };
}
