{ config, lib, pkgs, ... }:

let
  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";

  aliasFontLang = lang: family: font: ''
    <match target="pattern">
      <test name="lang">
        <string>${lang}</string>
      </test>
      <test name="family">
        <string>${family}</string>
      </test>
      <edit name="family" mode="prepend">
        <string>${font}</string>
      </edit>
    </match>
  '';

  editMode = name: type: value: ''
    <edit mode="assign" name="${name}">
      <${type}>${value}</${type}>
    </edit>
  '';
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

  home.file = lib.mkIf pkgs.stdenv.isLinux {
    ".config/fontconfig/conf.d/80-japanese.conf" = {
      text = ''
        <?xml version='1.0'?>
        <!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
        <fontconfig>
          ${aliasFontLang "ja" "serif" "Noto Serif CJK JP"}
          ${aliasFontLang "ja" "sans-serif" "Noto Sans CJK JP"}
        </fontconfig>
      '';
    };
    ".config/fontconfig/conf.d/99-antialias.conf" = {
      text = ''
        <?xml version='1.0'?>
        <!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
        <fontconfig>
          <match target="font">
            ${editMode "antialias" "bool" "true"}
            ${editMode "hinting" "bool" "true"}
            ${editMode "hintstyle" "const" "hintslight"}
            ${editMode "lcdfilter" "const" "lcddefault"}
            ${editMode "rgba" "const" "rgb"}
          </match>
        </fontconfig>
      '';
    };
  };
}
