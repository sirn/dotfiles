{
  flatpak.enable = true;

  imports = [
    ./ws.nix

    # programs
    ../programs/firefox.nix
    ../programs/sublime-text.nix
    ../programs/wezterm.nix
  ];
}
