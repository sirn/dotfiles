{ config, lib, pkgs, ... }:

let
  fuzzelcfg = config.programs.fuzzel;
in
{
  home.packages = with pkgs; [
    xwayland-satellite-stable
  ];

  systemd.user.services.xwayland-satellite = {
    Install.WantedBy = [ config.wayland.systemd.target ];

    Unit = {
      Description = "Xwayland outside your Wayland";
      BindsTo = [ config.wayland.systemd.target ];
      PartOf = [ config.wayland.systemd.target ];
      After = [ config.wayland.systemd.target ];
      Requisite = [ config.wayland.systemd.target ];
    };

    Service = {
      Type = "notify";
      NotifyAccess = "all";
      ExecStart = "${pkgs.xwayland-satellite-stable}/bin/xwayland-satellite :99";
      StandardOutput = "journal";
    };
  };

  programs.fuzzel = lib.mkIf fuzzelcfg.enable {
    settings = {
      main = {
        launch-prefix = "env DISPLAY=:99";
      };
    };
  };
}
