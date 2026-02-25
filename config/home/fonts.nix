{ config, lib, pkgs, ... }:

let
  defaultSansSerif = "Noto Sans";

  defaultSerif = "Noto Serif";

  defaultMonospace = "Hack";
in
{
  fonts = {
    fontconfig = {
      enable = true;

      defaultFonts = {
        sansSerif = [ defaultSansSerif ];
        serif = [ defaultSerif ];
        monospace = [ defaultMonospace ];
      };
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
    source-code-pro
    source-han-code-jp
    source-han-mono
    source-han-sans
    source-han-serif
    source-sans-pro
    source-serif-pro
    tlwg
    ubuntu-classic

    local.ia-writer-duo-static
    local.ia-writer-mono-static
    local.ia-writer-quattro-static
  ];

  # https://wiki.archlinux.org/title/Font_configuration
  gtk.gtk4.extraConfig = lib.mkIf pkgs.stdenv.isLinux {
    gtk-hint-font-metrics = true;
  };

  home.file = lib.mkIf pkgs.stdenv.isLinux {
    ".config/fontconfig/conf.d/21-emoji.conf" = {
      text = ''
        <?xml version='1.0'?>
        <!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
        <fontconfig>
          <!-- Prioritize Noto Color Emoji for emoji family -->
          <alias>
            <family>emoji</family>
            <prefer>
              <family>Noto Color Emoji</family>
            </prefer>
          </alias>

          <!-- Map emoji requests specifically -->
          <match target="pattern">
            <test name="family" compare="eq">
              <string>emoji</string>
            </test>
            <edit name="family" mode="prepend">
              <string>Noto Color Emoji</string>
            </edit>
          </match>

          <!-- Replace Noto Sans Symbols with color emoji for emoji ranges -->
          <match target="font">
            <test name="family" compare="eq">
              <string>Noto Sans Symbols</string>
            </test>
            <edit name="family" mode="prepend">
              <string>Noto Color Emoji</string>
            </edit>
          </match>
        </fontconfig>
      '';
    };
    ".config/fontconfig/conf.d/51-japanese.conf" = {
      text = ''
        <?xml version='1.0'?>
        <!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
        <fontconfig>
          <match target="pattern">
            <test name="lang">
              <string>ja</string>
            </test>
            <test name="family">
              <string>serif</string>
            </test>
            <edit name="family" mode="prepend" binding="strong">
              <string>Noto Serif CJK JP</string>
            </edit>
          </match>
          <match target="pattern">
            <test name="lang">
              <string>ja</string>
            </test>
            <test name="family">
              <string>sans-serif</string>
            </test>
            <edit name="family" mode="prepend" binding="strong">
              <string>Noto Sans CJK JP</string>
            </edit>
          </match>
        </fontconfig>
      '';
    };
    ".config/fontconfig/conf.d/51-thai.conf" = {
      text = ''
        <?xml version='1.0'?>
        <!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
        <fontconfig>
          <match target="pattern">
            <test name="lang">
              <string>th</string>
            </test>
            <test name="family">
              <string>serif</string>
            </test>
            <edit name="family" mode="prepend" binding="strong">
              <string>Norasi</string>
            </edit>
          </match>
          <match target="pattern">
            <test name="lang">
              <string>th</string>
            </test>
            <test name="family">
              <string>sans-serif</string>
            </test>
            <edit name="family" mode="prepend" binding="strong">
              <string>Waree</string>
            </edit>
          </match>
          <match target="pattern">
            <test name="lang">
              <string>th</string>
            </test>
            <test name="family">
              <string>monospace</string>
            </test>
            <edit name="family" mode="prepend" binding="strong">
              <string>Tlwg Typewriter</string>
            </edit>
          </match>
        </fontconfig>
      '';
    };
    ".config/fontconfig/conf.d/81-substitute.conf" = {
      text = ''
        <?xml version='1.0'?>
        <!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
        <fontconfig>
          <match target="pattern">
            <test name="family" compare="eq">
              <string>Arial</string>
            </test>
            <edit name="family" mode="assign" binding="strong">
              <string>${defaultSansSerif}</string>
            </edit>
          </match>
          <match target="pattern">
            <test name="family" compare="eq">
              <string>Courier New</string>
            </test>
            <edit name="family" mode="assign" binding="strong">
              <string>${defaultMonospace}</string>
            </edit>
          </match>
          <match target="pattern">
            <test name="family" compare="eq">
              <string>Helvetica Neue</string>
            </test>
            <edit name="family" mode="assign" binding="strong">
              <string>${defaultSansSerif}</string>
            </edit>
          </match>
          <match target="pattern">
            <test name="family" compare="eq">
              <string>Helvetica</string>
            </test>
            <edit name="family" mode="assign" binding="strong">
              <string>${defaultSansSerif}</string>
            </edit>
          </match>
          <match target="pattern">
            <test name="family" compare="eq">
              <string>Menlo</string>
            </test>
            <edit name="family" mode="assign" binding="strong">
              <string>${defaultMonospace}</string>
            </edit>
          </match>
          <match target="pattern">
            <test name="family" compare="eq">
              <string>Segoe UI</string>
            </test>
            <edit name="family" mode="assign" binding="strong">
              <string>${defaultSansSerif}</string>
            </edit>
          </match>
          <match target="pattern">
            <test name="family" compare="eq">
              <string>Tahoma</string>
            </test>
            <edit name="family" mode="assign" binding="strong">
              <string>${defaultSansSerif}</string>
            </edit>
          </match>
          <match target="pattern">
            <test name="family" compare="eq">
              <string>Times New Roman</string>
            </test>
            <edit name="family" mode="assign" binding="strong">
              <string>${defaultSerif}</string>
            </edit>
          </match>
          <match target="pattern">
            <test name="family" compare="eq">
              <string>Times</string>
            </test>
            <edit name="family" mode="assign" binding="strong">
              <string>${defaultSerif}</string>
            </edit>
          </match>
          <match target="pattern">
            <test name="family" compare="eq">
              <string>Verdana</string>
            </test>
            <edit name="family" mode="assign" binding="strong">
              <string>${defaultSansSerif}</string>
            </edit>
          </match>
        </fontconfig>
      '';
    };
    ".config/fontconfig/conf.d/99-antialias.conf" = {
      text = ''
        <?xml version='1.0'?>
        <!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>
        <fontconfig>
          <match target="font">
            <edit mode="assign" name="antialias">
              <bool>true</bool>
            </edit>
            <edit mode="assign" name="hinting">
              <bool>false</bool>
            </edit>
            <edit mode="assign" name="hintstyle">
              <const>hintnone</const>
            </edit>
            <edit mode="assign" name="lcdfilter">
              <const>lcddefault</const>
            </edit>
            <edit mode="assign" name="rgba">
              <const>rgb</const>
            </edit>
          </match>
        </fontconfig>
      '';
    };
  };
}
