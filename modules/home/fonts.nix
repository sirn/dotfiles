{ config, lib, pkgs, ... }:

let
  inherit (config.home) homeDirectory;
  inherit (pkgs.stdenv) isLinux;
  inherit (lib) mkIf;

  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";

  # fontconfig included with twemoji uses strong binding which broke
  # whitespace display in Firefox with monospace fonts.
  #
  # Also use the version from nixpkgs-unstable for newer emojis.
  twemojiWithoutFontconfig = pkgs.unstable.twemoji-color-font.overrideAttrs (orig: rec {
    installPhase = ''
      install -Dm755 TwitterColorEmoji-SVGinOT.ttf $out/share/fonts/truetype/TwitterColorEmoji-SVGinOT.ttf
    '';
  });
in
mkIf config.desktop.enable {
  fonts.fontconfig.enable = isLinux;

  home.packages = with pkgs; [
    fira
    fira-code
    fira-code-symbols
    fira-mono
    hack-font
    ia-writer-duospace
    ibm-plex
    inter
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
    twemojiWithoutFontconfig
    ubuntu_font_family

    local.ia-writer-duo-static
    local.ia-writer-mono-static
    local.ia-writer-quattro-static
  ];

  home.file = mkIf isLinux {
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
