{ pkgs, ... }:

{
  services.desktopManager.plasma6 = {
    enable = true;
    enableQt5Integration = true;
  };

  environment.plasma6.excludePackages = with pkgs.kdePackages; [
    elisa
    kate
    konsole
    oxygen
  ];

  # Required for syncing KDE settings to GTK, e.g. scaling.
  # No autostart; let kde-gtk-config start it.
  systemd.user.services = {
    xsettingsd = {
      enable = true;
      description = "XSETTINGS-protocol daemon";
      partOf = [ "graphical-session.target" ];

      serviceConfig = {
        ExecStart = "${pkgs.xsettingsd}/bin/xsettingsd";
        Slice = "session.slice";
      };
    };
  };

  environment.systemPackages = with pkgs; [
    # required for virtual keyboard support under wayland session
    maliit-keyboard

    # xsettingsd binary needs to be presented in PATH for
    # kde-gtk-config to be able to start it.
    xsettingsd

    # required for plasma to display HDR content in supported apps
    local.vulkan-hdr-layer
  ];

  environment.variables = {
    ENABLE_HDR_WSI = "1";
  };
}
