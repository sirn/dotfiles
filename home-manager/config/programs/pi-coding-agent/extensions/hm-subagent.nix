{ config, lib, ... }:

let
  agentsCfg = config.agents;

  # YAML-safe: quote strings containing colons, hashes, or other special chars
  yamlQuote = s: if builtins.match "^[a-zA-Z0-9_/., -]+$" s != null then s else "\"${s}\"";

  piAgentMdFiles = builtins.listToAttrs (
    builtins.attrValues (
      builtins.mapAttrs (
        name: agentCfg:
        let
          piCfg = agentCfg.pi;
          tools = builtins.concatStringsSep ", " piCfg.tools;
          content = ''
            ---
            name: ${name}
            description: ${yamlQuote agentCfg.description}${
              lib.optionalString (piCfg.runner == "claude-code") "\nrunner: claude-code"
            }
            tools: ${tools}
            model: ${piCfg.model}
            ${lib.optionalString (piCfg.thinkingLevel != null) "thinkingLevel: ${piCfg.thinkingLevel}"}
            ---
            ${agentsCfg.subagentPreamble}

            ${agentCfg.prompt}'';
        in
        {
          name = ".pi/agent/agents/${name}.md";
          value = {
            text = content;
          };
        }
      ) agentsCfg.subagents
    )
  );

  subagentListing =
    if agentsCfg.subagents == { } then
      ""
    else
      "\n"
      + ''
        ## Available Subagents

        Only the subagents listed below are available. Do not make up subagent names not in this list.

        ${builtins.concatStringsSep "\n" (
          builtins.map (
            name:
            let
              agentCfg = agentsCfg.subagents.${name};
            in
            "- **${name}**: ${agentCfg.description}"
            + "\n  - Tools: ${builtins.concatStringsSep ", " agentCfg.pi.tools}"
            + "\n  - Runner: ${agentCfg.pi.runner} (${agentCfg.pi.model})"
            + lib.optionalString (agentCfg.delegateWhen != null) "\n  - Delegate when: ${agentCfg.delegateWhen}"
          ) (builtins.attrNames agentsCfg.subagents)
        )}
      '';
in

{
  programs.pi-coding-agent = {
    extensionCustom."subagent".maxAgentsPerStep = 3;

    instructionText = lib.mkAfter subagentListing;
  };

  home.file = {
    ".pi/agent/extensions/hm-subagent".source = ../vendor/extensions/subagent;
  }
  // piAgentMdFiles;
}
