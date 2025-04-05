{ config, pkgs, lib, ... }:

let
  configHome =
    if pkgs.stdenv.isLinux
    then ".config/sublime-text"
    else "Library/Application Support/Sublime Text";

  sublimeConfig = {
    theme = "Meetio Theme.sublime-theme";
    color_scheme = "Meetio Darker.sublime-color-scheme";
    font_face = "PragmataPro Mono";
    font_size = if pkgs.stdenv.isDarwin then 14 else 12;
  };
in
{
  home.packages =
    if pkgs.stdenv.isLinux
    then with pkgs; [ unstable.sublime4 ]
    else [ ];

  home.file = {
    "${configHome}/Packages/Meetio Theme".source = pkgs.fetchFromGitHub {
      owner = "meetio-theme";
      repo = "sublime-meetio-theme";
      rev = "4070-7.5.0";
      sha256 = "sha256-cyfihghCrCj0ctCATFlo4A2RVLy7Rs6cxPGueSSKURQ=";
    };

    "${configHome}/Packages/User/Preferences.sublime-settings" = {
      text = builtins.toJSON sublimeConfig;
    };
  };
}
