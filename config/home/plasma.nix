{ config, lib, pkgs, ... }:

{
  home = {
    sessionVariables = {
      QT_SCALE_FACTOR_ROUNDING_POLICY = "RoundPreferFloor";
    };

    pointerCursor = {
      gtk.enable = true;
      x11.enable = true;
      package = pkgs.kdePackages.breeze;
      name = "breeze_cursors";
      size = 24;
    };
  };

  programs.firefox = lib.mkIf config.programs.firefox.enable {
    nativeMessagingHosts = [
      pkgs.kdePackages.plasma-browser-integration
    ];

    profiles.main.extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
      plasma-integration
    ];
  };
}
