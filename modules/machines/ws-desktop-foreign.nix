{
  machine.isNixOS = false;

  imports = [
    ./ws.nix

    # programs
    ../programs/wezterm.nix
  ];
}
