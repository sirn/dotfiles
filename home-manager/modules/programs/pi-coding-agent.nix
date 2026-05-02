{
  lib,
  pkgs,
  config,
  ...
}:

let
  cfg = config.programs.pi-coding-agent;
in
{
  options.programs.pi-coding-agent = {
    enable = lib.mkEnableOption "Pi coding agent";

    extensions = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = ''
        List of extension file paths to load.
        Extensions are TypeScript modules that extend Pi's behavior.
      '';
    };

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.local.pi-coding-agent;
      description = "The Pi coding agent package to use.";
    };

    instructionText = lib.mkOption {
      type = lib.types.lines;
      default = "";
      description = ''
        The instruction text to use as AGENTS.md.
      '';
    };

    settings = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Pi settings written to settings.json (extensions auto-merged).";
    };

    providers = lib.mkOption {
      type = lib.types.attrs;
      default = { };
      description = "Provider configurations for models.json.";
    };

    keybindings = lib.mkOption {
      type = lib.types.attrsOf (lib.types.listOf lib.types.str);
      default = { };
      description = ''
        Keyboard shortcuts configuration written to keybindings.json.
        Each action maps to a list of key combinations.
        See https://github.com/badlogic/pi-mono/blob/main/docs/keybindings.md
      '';
      example = lib.literalExpression ''
        {
          "cursorUp" = [ "up" "ctrl+p" ];
          "newLine" = [ "shift+enter" "ctrl+j" ];
        }
      '';
    };

    extensionCustom = lib.mkOption {
      type = lib.types.attrsOf (pkgs.formats.json { }).type;
      default = { };
      description = ''
        Per-extension custom configuration.
        Each attribute name becomes a directory under ~/.pi/agent/custom/<name>/
        with the attribute value written as config.json.
        Values may be any JSON-compatible value supported by builtins.toJSON.
      '';
      example = lib.literalExpression ''
        {
          "execution-policy" = { shellPolicy.autoMode.enable = true; };
          "smart-compact" = {
            model = "gemini-3-flash-preview";
            provider = "plexus";
          };
        }
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    programs.git.ignores = [ ".pi/" ];

    home.file = {
      ".pi/agent/settings.json".text = builtins.toJSON (cfg.settings // { extensions = cfg.extensions; });
      ".pi/agent/models.json".text = builtins.toJSON { providers = cfg.providers; };
      ".pi/agent/AGENTS.md".text = cfg.instructionText;
    }
    // lib.optionalAttrs (cfg.keybindings != { }) {
      ".pi/agent/keybindings.json".text = builtins.toJSON cfg.keybindings;
    }
    // lib.mapAttrs' (
      name: value:
      lib.nameValuePair ".pi/agent/custom/${name}/config.json" { text = builtins.toJSON value; }
    ) cfg.extensionCustom;
  };
}
