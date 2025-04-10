{
  flatpak.enable = true;

  imports = [
    ./ws.nix

    # programs
    ../programs/wezterm.nix
    ../programs/sublime-text.nix
  ];
}
