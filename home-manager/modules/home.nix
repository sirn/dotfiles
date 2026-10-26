{ config, lib, ... }:

let
  inherit (lib) mkOption types;
in
{
  options.home = {
    wrapLauncher = mkOption {
      type = types.functionTo types.path;
      default = x: if lib.isDerivation x then lib.getExe x else x;
      description = ''
        Function to wrap a launcher command. Accepts either a package
        (will use lib.getExe) or a path string.
      '';
    };
  };

  config = {
    lib.home.wrapLauncher = config.home.wrapLauncher;
  };
}
