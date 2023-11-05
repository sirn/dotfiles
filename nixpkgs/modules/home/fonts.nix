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
{
  fonts.fontconfig.enable = isLinux;

  home.packages = with pkgs; [
    # Fira family
    fira
    fira-mono
    fira-code
    fira-code-symbols

    # Source Pro family
    source-code-pro
    source-sans-pro
    source-serif-pro

    # Source Han family
    source-han-sans
    source-han-mono
    source-han-serif
    source-han-code-jp

    # Noto family
    noto-fonts
    noto-fonts-extra

    # Inter family
    inter

    # Hack family
    hack-font

    # Twemoji family
    twemojiWithoutFontconfig

    # IBM Plex family
    ibm-plex

    # Ubuntu family
    ubuntu_font_family

    # IPA fonts
    ipafont
    ipaexfont

    # iA fonts
    ia-writer-duospace
    local.ia-writer-duo-static
    local.ia-writer-mono-static
    local.ia-writer-quattro-static

    # Droid Sans Thai
    local.droid-sans-thai
    local.droid-serif-thai
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
