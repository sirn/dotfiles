{ config, lib, pkgs, ... }:

let
  inherit (config.home) homeDirectory;
  inherit (pkgs.stdenv) isLinux;
  inherit (lib) mkIf;

  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
in
{
  fonts.fontconfig.enable = isLinux;

  home.packages = with pkgs; [
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

    # Hack family
    hack-font

    # Twemoji family
    twemoji-color-font

    # IBM Plex family
    ibm-plex

    # IPA fonts
    ipafont

    # Takao fonts
    takao

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
