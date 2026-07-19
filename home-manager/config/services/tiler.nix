{ ... }:

{
  services.tiler = {
    enable = true;
    settings.layout.alwaysCenterSingleColumn = true;
    settings.keybinds = [
      {
        keys = "ctrl+alt+d";
        action = "dump_frame_state";
      }
    ];
  };
}
