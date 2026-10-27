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

    enableStarshipIntegration = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Show the zmx session name in the starship prompt.";
    };
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

    programs.starship.settings = lib.optionalAttrs cfg.enableStarshipIntegration {
      custom.zmx = {
        when = ''[ -n "$ZMX_SESSION" ]'';
        command = ''echo "$ZMX_SESSION"'';
        format = "[zmx:$output]($style) ";
        style = "yellow";
      };
    };
  };
}
