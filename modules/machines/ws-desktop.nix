{
  machine.isNixOS = false;

  imports = [
    ../common.nix
    ../common-linux.nix

    # programs
    ../programs/wezterm.nix
  ];
}
