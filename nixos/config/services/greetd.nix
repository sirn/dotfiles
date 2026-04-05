{
  lib,
  config,
  pkgs,
  ...
}:

let
  regreetSwayCfg = pkgs.writeText "sway-regreet-config" ''
    exec "${lib.getExe config.programs.regreet.package}; ${config.programs.sway.package}/bin/swaymsg exit"

    exec ${lib.getExe pkgs.swayidle} -w \
        timeout 180 '${lib.getExe' config.programs.sway.package "swaymsg"} "output * dpms off"' \
        resume '${lib.getExe' config.programs.sway.package "swaymsg"} "output * dpms on"'

    include /etc/sway/config.d/*
  '';
in
{
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${lib.getExe config.programs.sway.package} --config ${regreetSwayCfg}";
      };
    };
  };

  programs.regreet = {
    enable = true;

    font = {
      name = "Noto Sans";
      size = 10;
    };

    cursorTheme = {
      name = "breeze_cursors";
      package = pkgs.kdePackages.breeze;
    };

    theme = {
      name = lib.mkDefault "Breeze";
      package = pkgs.kdePackages.breeze-gtk;
    };

    iconTheme = {
      name = lib.mkDefault "breeze";
      package = pkgs.kdePackages.breeze-icons;
    };

    settings = {
      background = {
        path = "${pkgs.nixos-artwork.wallpapers.nineish-catppuccin-mocha}/share/backgrounds/nixos/nix-wallpaper-nineish-catppuccin-mocha.png";
        fit = "Cover";
      };

      GTK = {
        application_prefer_dark_theme = true;
      };
    };

    extraCss = ''
      frame {
        border-radius: 5px;
      }

      frame.top {
        padding: 5px;
        border-radius: 0 0 5px 5px;
      }
    '';
  };
}
