{ pkgs, config, ... }:

{
  flatpak.enable = true;

  imports = [
    ../common.nix
    ../common-linux.nix

    # profiles
    ../home/flatpak.nix
    ../home/fonts.nix
    ../home/plasma.nix

    # programs
    ../programs/firefox.nix
    ../programs/intellij.nix
    ../programs/looking-glass-client.nix
    ../programs/sublime-text.nix
    ../programs/thunderbird.nix
    ../programs/wezterm.nix

    # services
    ../services/dropbox.nix
  ];

  programs.looking-glass-client.package = pkgs.local.looking-glass-client_b7;
}
