{ lib, config, ... }:

{
  flatpak.enable = true;

  imports = [
    ../common.nix
    ../common-linux.nix

    # profiles
    ../home/breeze.nix
    ../home/flatpak.nix
    ../home/fonts.nix
    ../home/i18n.nix
    ../home/laptop.nix
    ../home/niri.nix
    ../home/notification.nix
    ../home/sway.nix
    ../home/uwsm.nix

    # programs
    ../programs/bitwarden.nix
    ../programs/firefox.nix
    ../programs/imagemagick.nix
    ../programs/mcp.nix
    ../programs/obsidian.nix
    ../programs/sublime-text.nix
    ../programs/wezterm.nix

    # services
    ../services/languagetool.nix
    ../services/swww.nix
  ];

  services.kanshi = lib.mkIf config.services.kanshi.enable {
    settings = [
      {
        output = {
          alias = "u3425we";
          criteria = "Dell Inc. DELL U3425WE 7WWR3Z3";
          mode = "3440x1440@120Hz";
          position = "0,0";
          adaptiveSync = true;
          scale = 1.0;
        };
      }
      {
        output = {
          alias = "system76";
          criteria = "Chimei Innolux Corporation 0x148A Unknown";
          position = "0,0";
          mode = "1920x1200";
          adaptiveSync = true;
          scale = 1.0;
        };
      }
      {
        profile = {
          name = "dual_system76_aw3225qf";
          outputs = [
            {
              criteria = "$aw3225qf";
              status = "enable";
            }
            {
              criteria = "$system76";
              status = "disable";
            }
          ];
        };
      }
      {
        profile = {
          name = "dual_system76_u3425we";
          outputs = [
            {
              criteria = "$system76";
              status = "disable";
            }
            {
              criteria = "$u3425we";
              status = "enable";
              position = "0,0";
              mode = "3440x1440@120Hz";
              adaptiveSync = true;
              scale = 1.0;
            }
          ];
        };
      }
      {
        profile = {
          name = "only_system76";
          outputs = [
            {
              criteria = "$system76";
              status = "enable";
            }
          ];
        };
      }
    ];
  };
}
