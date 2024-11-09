{ pkgs, lib, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isDarwin isLinux;

  sublimeConfig = {
    theme = "Adaptive.sublime-theme";
    color_scheme = "Mariana.sublime-color-scheme";
    font_face = "PragmataPro Mono";
    font_size = if isDarwin then 14 else 12;
  };
in
{
  home.packages =
    if isLinux
    then with pkgs; [ unstable.sublime4 ]
    else [ ];

  xdg.configFile = mkIf isLinux {
    "sublime-text/Packages/User/Preferences.sublime-settings" = {
      text = builtins.toJSON sublimeConfig;
    };
  };

  home.file = mkIf isDarwin {
    "Library/Application Support/Sublime Text/Packages/User/Preferences.sublime-settings" = {
      text = builtins.toJSON sublimeConfig;
    };
  };
}
