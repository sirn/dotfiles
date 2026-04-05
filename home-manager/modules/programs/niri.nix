{ lib, config, ... }:

let
  cfg = config.programs.niri;
in
{
  options = {
    programs = {
      niri = {
        systemd = {
          enable = lib.mkEnableOption "enable the niri systemd user service" // {
            default = cfg.enable;
            defaultText = lib.literalExpression "programs.niri.enable";
          };
        };
      };
    };
  };
}
