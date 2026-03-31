{
  config,
  lib,
  pkgs,
  ...
}:

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

    shell = {
      interactiveShell = mkOption {
        type = types.str;
        default = lib.getExe pkgs.bash;
        description = ''
          Default interactive shell.
        '';
      };
    };
  };

  config = {
    lib.home.wrapLauncher = config.home.wrapLauncher;
  };
}
