{
  zramSwap.enable = true;

  # NixOS has systemd.oomd.enable enabled by default
  # but we set it anyway to ensure it is really enabled.
  # See: https://wiki.nixos.org/wiki/Swap
  systemd.oomd.enable = true;
}
