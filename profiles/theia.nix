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

      ];
    };
}
