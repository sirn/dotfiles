{ pkgs, ... }:

let
  inherit (pkgs) formats;
  jsonFormat = formats.json { };
in
{
  home.packages = with pkgs; [
    unstable.zed-editor
  ];

  xdg.configFile = {
    "zed/settings.json" = {
      source = jsonFormat.generate "settings.json" {
        theme = "Rosé Pine";
        vim_mode = true;
        ui_font_size = 16;
        buffer_font_size = 16;
        auto_update = false;
      };
    };
  };
}
