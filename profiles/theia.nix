{
  home =
    { lib, config, ... }:
    {
      launchd.enable = true;

      imports = [
        # common
        ../home-manager/config/common.nix
        ../home-manager/config/common-darwin.nix

        # profiles
        ../home-manager/config/home/sops.nix
        ../home-manager/config/home/shell.nix
        ../home-manager/config/home/xdg.nix

        # services
        ../home-manager/config/services/coord.nix
        ../home-manager/config/services/tiler.nix
      ];
    };
}
