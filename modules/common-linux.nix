{
  imports = [
    ./home/desktop.nix
    ./home/flatpak.nix
    ./home/fonts.nix
    ./home/sway.nix

    # programs
    ./programs/distrobox.nix
    ./programs/firefox.nix
    ./programs/foot.nix
    ./programs/intellij.nix
    ./programs/looking-glass-client.nix
    ./programs/mpv.nix
    ./programs/s-tui.nix
    ./programs/thunderbird.nix
  ];
}
