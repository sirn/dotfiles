{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.zmx;
  zmx = pkgs.unstable.zmx;
in
{
  options.programs.zmx = {
    enable = lib.mkEnableOption "zmx session manager";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      (
        if pkgs.stdenv.isLinux then
          pkgs.writeShellScriptBin "zmx" ''
            exec ${pkgs.systemd}/bin/systemd-run \
              --user \
              --scope \
              --quiet \
              --slice=app.slice \
              -- \
              ${lib.getExe zmx} "$@"
          ''
        else
          zmx
      )
    ];
  };
}
