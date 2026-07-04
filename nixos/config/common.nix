{
  imports = [
    # system
    ./system/apparmor.nix
    ./system/doas.nix
    ./system/locale.nix
    ./system/nix.nix
    ./system/sudo.nix
    ./system/system.nix
    ./system/user.nix

    # programs
    ./programs/age.nix
    ./programs/attic.nix
    ./programs/git.nix
    ./programs/mosh.nix
    ./programs/sops.nix

    # services
    ./services/avahi.nix
    ./services/chrony.nix
    ./services/openssh.nix
  ];
}
