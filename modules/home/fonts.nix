{ config, lib, pkgs, ... }:

let
  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
in
{
  fonts.fontconfig.enable = pkgs.stdenv.isLinux;

  home.packages = with pkgs; [
    fira
    fira-code
    fira-code-symbols
    fira-mono
    hack-font
    ia-writer-duospace
    ibm-plex
    inter
    iosevka
    ipaexfont
    ipafont
    noto-fonts
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

  home.file = lib.mkIf pkgs.stdenv.isLinux {
    ".config/fontconfig/fonts.conf" = {
      text = ''
        <?xml version='1.0'?>
        <!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
        <fontconfig>
          <include ignore_missing="yes">/etc/fonts/fonts.conf</include>
          <include ignore_missing="yes">${config.home.homeDirectory}/.config/fontconfig/conf.d</include>
        </fontconfig>
      '';
    };
    ".config/fontconfig/conf.d/99-dotfiles.conf" = {
      text = ''
        <?xml version='1.0'?>
        <!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
        <fontconfig>
          <include ignore_missing="yes">${dotfilesDir}/etc/fontconfig/conf.d</include>
        </fontconfig>
      '';
    };
  };
}
