{
  config,
  lib,
  pkgs,
  ...
}:

let
  uwsmCfg = config.programs.uwsm;
in
{
  xdg.portal.config.sway = {
    default = [ "gtk" ];
    "org.freedesktop.impl.portal.ScreenCast" = "wlr";
    "org.freedesktop.impl.portal.Screenshot" = "wlr";
    "org.freedesktop.impl.portal.Inhibit" = "none";
  };

  programs.uwsm.waylandCompositors = lib.mkIf uwsmCfg.enable {
    sway = {
      prettyName = "Sway";
      comment = "Sway compositor managed by UWSM";
      binPath = pkgs.writeShellScript "sway" ''
        exec sway
      '';
    };
  };
}
