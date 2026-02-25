{
  flatpak.applications = {
    "md.obsidian.Obsidian" = {
      overrides = {
        sockets = [
          "wayland"
        ];

        environment = {
          OBSIDIAN_USE_WAYLAND = "1";
        };
      };
    };
  };
}
