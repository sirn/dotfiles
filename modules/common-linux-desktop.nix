{
  imports = [
    ./home/flatpak.nix
    ./home/fonts.nix
    ./home/sway.nix

    # programs
    ./programs/firefox.nix
    ./programs/intellij.nix
    ./programs/looking-glass-client.nix
    ./programs/mpv.nix
    ./programs/s-tui.nix
    ./programs/thunderbird.nix
  ];
}
