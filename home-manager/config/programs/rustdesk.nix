{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.packages = lib.mkIf (!config.flatpak.enable) [ pkgs.rustdesk-flutter ];

  flatpak.applications = {
    "com.rustdesk.RustDesk" = {
      overrides = {
        sockets = [ "wayland" ];
        environment = {
          GDK_BACKEND = "wayland";
        };
      };
    };
  };
}
