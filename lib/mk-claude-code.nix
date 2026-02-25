{ config, lib, pkgs }:

let
  cfg = config.programs.claude-code;
  jsonFormat = pkgs.formats.json { };
in
{ name, configDir, extraScript ? "" }: {
  files = {
    "${configDir}/settings.json".source = jsonFormat.generate "claude-code-settings.json" (
      cfg.settings // {
        "$schema" = "https://json.schemastore.org/claude-code-settings.json";
      }
    );
  }
  // lib.optionalAttrs (cfg.memory.text != null) {
    "${configDir}/CLAUDE.md".text = cfg.memory.text;
  }
  // lib.optionalAttrs (cfg.memory.source != null) {
    "${configDir}/CLAUDE.md".source = cfg.memory.source;
  }
  // lib.mapAttrs'
    (agentName: content:
      lib.nameValuePair "${configDir}/agents/${agentName}.md" (
        if lib.isPath content
        then { source = content; }
        else { text = content; }
      )
    )
    cfg.agents
  // { "${configDir}/skills".source = ../var/agents/skills; };

  wrapper = pkgs.writeScriptBin "claude-${name}" ''
    #!${pkgs.bash}/bin/bash
    export CLAUDE_CONFIG_DIR=$HOME/${configDir}
    ${extraScript}
    exec ${lib.getExe cfg.finalPackage} "$@"
  '';
}
