{ lib, pkgs, ... }:

{
  hardware = {
    graphics = {
      enable = true;
    };
  };

  # uses by pipewire, not required but recommended
  security.rtkit.enable = true;

  programs.dconf = {
    enable = true;
  };

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    audio.enable = true;
    jack.enable = true;
    pulse.enable = true;

    wireplumber = {
      enable = true;
      extraConfig = {
        "monitor.bluez.properties" = {
          "bluez5.enable-sbc-xq" = true;
          "bluez5.enable-msbc" = true;
          "bluez5.enable-hw-volume" = true;
          "bluez5.roles" = [
            "hsp_hs"
            "hsp_ag"
            "hfp_hf"
            "hfp_ag"
          ];
        };
      };
    };
  };

  fonts.fontconfig = {
    # Looking around, it seems like this was disabled by default
    # to workaround some Microsoft fonts defaulting to bitmap.
    # Since we don't care about Microsoft, and emoji fonts are
    # considered embeddedbitmap, it makes more sense to enable this.
    useEmbeddedBitmaps = true;
  };

  i18n.inputMethod = {
    enable = true;
    type = "fcitx5";

    fcitx5 = {
      waylandFrontend = true;
      addons = with pkgs; [
        fcitx5-gtk
        fcitx5-mozc
      ];
    };
  };

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [
      kdePackages.xdg-desktop-portal-kde
      xdg-desktop-portal-gtk
      xdg-desktop-portal-wlr
    ];
  };
}
