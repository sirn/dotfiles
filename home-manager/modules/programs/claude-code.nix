{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.claude-code;
  jsonFormat = pkgs.formats.json { };

  mkSourceEntry = content: if lib.isPath content then { source = content; } else { text = content; };

  mkMarkdownEntries =
    configDir: subdir: attrs:
    lib.mapAttrs' (
      name: content: lib.nameValuePair "${configDir}/${subdir}/${name}.md" (mkSourceEntry content)
    ) attrs;

  mkSkillEntry =
    configDir: name: content:
    if lib.hm.strings.isPathLike content && lib.pathIsDirectory content then
      lib.nameValuePair "${configDir}/skills/${name}" {
        source = content;
        recursive = true;
      }
    else
      lib.nameValuePair "${configDir}/skills/${name}/SKILL.md" (
        if lib.hm.strings.isPathLike content then { source = content; } else { text = content; }
      );

  renderFiles =
    name: profile:
    let
      inherit (profile) configDir;

      skillFiles =
        if lib.hm.strings.isPathLike profile.skills then
          {
            "${configDir}/skills" = {
              source = profile.skills;
              recursive = true;
            };
          }
        else
          lib.mapAttrs' (mkSkillEntry configDir) profile.skills;
    in
    {
      "${configDir}/settings.json".source = jsonFormat.generate "claude-code-${name}-settings.json" (
        profile.settings // { "$schema" = "https://json.schemastore.org/claude-code-settings.json"; }
      );
    }
    // lib.optionalAttrs (lib.isPath profile.context) {
      "${configDir}/CLAUDE.md".source = profile.context;
    }
    // lib.optionalAttrs (!lib.isPath profile.context && profile.context != "") {
      "${configDir}/CLAUDE.md".text = profile.context;
    }
    // mkMarkdownEntries configDir "agents" profile.agents
    // mkMarkdownEntries configDir "commands" profile.commands
    // mkMarkdownEntries configDir "rules" profile.rules
    // lib.mapAttrs' (
      hookName: content:
      lib.nameValuePair "${configDir}/hooks/${hookName}" {
        text = content;
        executable = true;
      }
    ) profile.hooks
    // skillFiles;

  mkWrapper =
    name: profile:
    let
      pluginArgs = lib.concatMap (plugin: [
        "--plugin-dir"
        "${plugin}"
      ]) profile.plugins;
    in
    pkgs.writeScriptBin "claude-${name}" ''
      #!${pkgs.runtimeShell}
      export CLAUDE_CONFIG_DIR="$HOME/${profile.configDir}"
      ${profile.extraScript}
      exec ${lib.getExe cfg.finalPackage} ${lib.escapeShellArgs pluginArgs} "$@"
    '';

  mkProfileOptions = name: {
    configDir = lib.mkOption {
      type = lib.types.str;
      default = ".claude-${name}";
      description = "Directory holding this profile's Claude Code configuration, relative to the home directory.";
    };

    settings = lib.mkOption {
      inherit (jsonFormat) type;
      default = cfg.settings;
      description = "JSON configuration for this profile's settings.json.";
    };

    context = lib.mkOption {
      type = lib.types.either lib.types.lines lib.types.path;
      default = cfg.context;
      description = "Global context written to CLAUDE.md for this profile.";
    };

    agents = lib.mkOption {
      type = lib.types.attrsOf (lib.types.either lib.types.lines lib.types.path);
      default = cfg.agents;
      description = "Custom agents for this profile.";
    };

    commands = lib.mkOption {
      type = lib.types.attrsOf (lib.types.either lib.types.lines lib.types.path);
      default = cfg.commands;
      description = "Custom commands for this profile.";
    };

    rules = lib.mkOption {
      type = lib.types.attrsOf (lib.types.either lib.types.lines lib.types.path);
      default = cfg.rules;
      description = "Modular rules for this profile.";
    };

    hooks = lib.mkOption {
      type = lib.types.attrsOf lib.types.lines;
      default = cfg.hooks;
      description = "Custom hooks for this profile.";
    };

    skills = lib.mkOption {
      type = lib.types.either (lib.types.attrsOf (
        lib.types.oneOf [
          lib.types.lines
          lib.types.path
          lib.types.str
        ]
      )) lib.types.path;
      default = cfg.skills;
      description = "Custom skills for this profile.";
    };

    plugins = lib.mkOption {
      type = lib.types.listOf (lib.types.either lib.types.package lib.types.path);
      default = [ ];
      description = "Plugins passed to this profile with --plugin-dir.";
    };

    extraScript = lib.mkOption {
      type = lib.types.lines;
      default = "";
      description = "Extra shell lines run before the profile starts.";
    };
  };
in
{
  options.programs.claude-code.extraProfiles = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule ({ name, ... }: { options = mkProfileOptions name; })
    );
    default = { };
    description = ''
      Additional Claude Code profiles.

      Each profile gets its own configuration directory, settings, context, and
      wrapper named claude-<name>. Files and the wrapper are installed
      automatically.
    '';
  };

  config = lib.mkIf (cfg.enable && cfg.extraProfiles != { }) {
    home.file = lib.concatMapAttrs renderFiles cfg.extraProfiles;
    home.packages = lib.mapAttrsToList mkWrapper cfg.extraProfiles;

    agents.sandbox.extraWritePaths = lib.mapAttrsToList (
      _: profile: "${config.home.homeDirectory}/${profile.configDir}"
    ) cfg.extraProfiles;
  };
}
