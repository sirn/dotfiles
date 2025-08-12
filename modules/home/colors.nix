let
  colorScheme = {
    background = "#242424";
    foreground = "#ffffff";
    selection = "#d1d1d1";
    scrollbar = "#d1d1d1";

    normal = {
      black = "#242424";
      red = "#f62b5a";
      green = "#47b413";
      yellow = "#e3c401";
      blue = "#24acd4";
      magenta = "#f2affd";
      cyan = "#13c299";
      white = "#e6e6e6";
    };

    bright = {
      black = "#616161";
      red = "#ff4d51";
      green = "#35d450";
      yellow = "#e9e836";
      blue = "#5dc5f8";
      magenta = "#feabf2";
      cyan = "#24dfc4";
      white = "#ffffff";
    };
  };

  stripHash = color: builtins.substring 1 6 color;
in
{
  programs.alacritty = {
    settings = {
      colors = {
        primary = {
          background = colorScheme.background;
          foreground = colorScheme.foreground;
        };

        normal = colorScheme.normal;
        bright = colorScheme.bright;
      };
    };
  };

  programs.foot = {
    settings = {
      colors = {
        background = stripHash colorScheme.background;
        foreground = stripHash colorScheme.foreground;
        regular0 = stripHash colorScheme.normal.black;
        regular1 = stripHash colorScheme.normal.red;
        regular2 = stripHash colorScheme.normal.green;
        regular3 = stripHash colorScheme.normal.yellow;
        regular4 = stripHash colorScheme.normal.blue;
        regular5 = stripHash colorScheme.normal.magenta;
        regular6 = stripHash colorScheme.normal.cyan;
        regular7 = stripHash colorScheme.normal.white;
        bright0 = stripHash colorScheme.bright.black;
        bright1 = stripHash colorScheme.bright.red;
        bright2 = stripHash colorScheme.bright.green;
        bright3 = stripHash colorScheme.bright.yellow;
        bright4 = stripHash colorScheme.bright.blue;
        bright5 = stripHash colorScheme.bright.magenta;
        bright6 = stripHash colorScheme.bright.cyan;
        bright7 = stripHash colorScheme.bright.white;
      };
    };
  };

  programs.wezterm = {
    colorSchemes = {
      default = {
        ansi = [
          colorScheme.normal.black
          colorScheme.normal.red
          colorScheme.normal.green
          colorScheme.normal.yellow
          colorScheme.normal.blue
          colorScheme.normal.magenta
          colorScheme.normal.cyan
          colorScheme.normal.white
        ];
        brights = [
          colorScheme.bright.black
          colorScheme.bright.red
          colorScheme.bright.green
          colorScheme.bright.yellow
          colorScheme.bright.blue
          colorScheme.bright.magenta
          colorScheme.bright.cyan
          colorScheme.bright.white
        ];

        background = colorScheme.background;
        cursor_bg = colorScheme.foreground;
        cursor_border = colorScheme.foreground;
        cursor_fg = colorScheme.background;
        foreground = colorScheme.foreground;
        scrollbar_thumb = colorScheme.scrollbar;
        selection_bg = colorScheme.selection;
        selection_fg = colorScheme.background;
      };
    };
  };
}
