{
  imports = [
    ../programs/gamemode.nix
    ../programs/gamescope.nix
  ];

  hardware.steam-hardware.enable = true;
  hardware.xpadneo.enable = true;
}
