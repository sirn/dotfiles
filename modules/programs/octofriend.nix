{ lib, config, pkgs, ... }:

let
  cfg = config.programs.octofriend;
in
{
  options = {
    programs = {
      octofriend = {
        enable = lib.mkEnableOption "enable octofriend";

        package = lib.mkPackageOption pkgs [ "local" "octofriend" ] { };

        instruction = lib.mkOption {
          type = lib.types.lines;
          default = "";
          description = ''
            Instruction text for Octofriend.
          '';
        };
      };
    };
  };

  config = {
    programs.octofriend = {
      enable = true;

      package = (pkgs.writeScriptBin "octo" ''
        #!${pkgs.runtimeShell}
        exec "${lib.getExe pkgs.local.envWrapper}" \
          -i ~/.config/llm-agent/env \
          -- "${pkgs.local.octofriend}/bin/octo" "$@"
      '');
    };

    home = lib.mkIf cfg.enable {
      packages = lib.mkIf (cfg.package != null) [
        cfg.package
      ];

      file = lib.mkIf (cfg.instruction != "") {
        ".config/octofriend/OCTO.md" = {
          text = cfg.instruction;
        };
      };
    };
  };
}
