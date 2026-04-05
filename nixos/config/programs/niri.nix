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
  xdg.portal.config.niri = {
    default = [ "gtk" ];
    "org.freedesktop.impl.portal.ScreenCast" = "wlr";
    "org.freedesktop.impl.portal.Screenshot" = "wlr";
    "org.freedesktop.impl.portal.Inhibit" = "none";
  };

  programs.uwsm.waylandCompositors = lib.mkIf uwsmCfg.enable {
    niri = {
      prettyName = "Niri";
      comment = "Niri compositor managed by UWSM";
      binPath = pkgs.writeShellScript "niri" ''
        exec niri --session
      '';
    };
  };
}
