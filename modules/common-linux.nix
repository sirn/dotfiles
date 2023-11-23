{ machine, ... }:

{
  imports = [
    ./home/flatpak.nix
    ./home/fonts.nix
    ./home/sway.nix

    # programs
    ./programs/s-tui.nix
    ./programs/firefox.nix
    ./programs/intellij.nix
    ./programs/looking-glass-client.nix
    ./programs/mpv.nix
    ./programs/thunderbird.nix
  ];
}
