{ config, pkgs, lib, ... }:

let
  configHome =
    if pkgs.stdenv.isLinux
    then ".config/sublime-text"
    else "Library/Application Support/Sublime Text";

  sublimeConfig = {
    theme = "Meetio Theme.sublime-theme";
    color_scheme = "Meetio Darker.sublime-color-scheme";
    font_face = "PragmataPro Mono Liga";
    font_size = if pkgs.stdenv.isDarwin then 14 else 12;
    update_check = false;
  };
in
{
  home.packages =
    if pkgs.stdenv.isLinux
    then with pkgs; [ sublime4 ]
    else [ ];

  home.file = {
    "${configHome}/Packages/Meetio Theme".source = pkgs.fetchFromGitHub {
      owner = "meetio-theme";
      repo = "sublime-meetio-theme";
      rev = "4070-7.5.0";
      sha256 = "sha256-cyfihghCrCj0ctCATFlo4A2RVLy7Rs6cxPGueSSKURQ=";
    };

    "${configHome}/Packages/Nix".source = pkgs.fetchFromGitHub {
      owner = "wmertens";
      repo = "sublime-nix";
      rev = "V2.4.0";
      sha256 = "sha256-gZFUFSiY2MxiEiiPDekiyYfQlY8+zUhXb0Y98xCpqrs=";
    };

    "${configHome}/Packages/LSP".source = pkgs.fetchFromGitHub {
      owner = "sublimelsp";
      repo = "LSP";
      rev = "4070-2.3.0";
      sha256 = "sha256-tVG5Qlvtfyh+GKvXusIO0ysCjSSo9Udp+3Ej1qR1cDA=";
    };

    "${configHome}/Packages/Home/Preferences.sublime-settings" = {
      text = builtins.toJSON sublimeConfig;
    };
  };
}
