{ config, lib, pkgs, ... }:

let
  instructionText = builtins.readFile ../../var/agents/instruction.md;

  skillsDir = ../../var/agents/skills;

  agentsDir = ../../var/agents/agents;

  permissionsPolicy = builtins.fromTOML (builtins.readFile ../../var/agents/permissions.toml);

  effectivePolicy = mode:
    let
      default = permissionsPolicy.default or { };
      modePolicy = permissionsPolicy.mode.${mode} or { };
      mergeLists = lists: lib.unique (lib.concatLists (lib.filter (l: l != null) lists));
      mergeCmds = section:
        let
          defaultShell = (default.commands.${section} or { }).shell or [ ];
          modeShell = ((modePolicy.commands or { }).${section} or { }).shell or [ ];
        in
        { shell = mergeLists [ defaultShell modeShell ]; };
    in
    {
      tools = default.tools // (modePolicy.tools or { });
      commands = {
        allow = mergeCmds "allow";
        ask = mergeCmds "ask";
        deny = mergeCmds "deny";
      };
      paths = default.paths;
    };

  toOpencodePermissions = mode:
    let
      policy = effectivePolicy mode;
      inherit (policy) tools commands paths;

      mkPathRules = section:
        lib.listToAttrs (map (p: lib.nameValuePair p "allow") (paths.allow.${section} or [ ]))
        // lib.listToAttrs (map (p: lib.nameValuePair p "deny") (paths.deny.${section} or [ ]))
        // { "*" = "allow"; };

      mkBashRules =
        let
          allows = map (cmd: lib.nameValuePair "${cmd} *" "allow") (commands.allow.shell or [ ])
            ++ map (cmd: lib.nameValuePair cmd "allow") (commands.allow.shell or [ ]);
          asks = map (cmd: lib.nameValuePair "${cmd} *" "ask") (commands.ask.shell or [ ])
            ++ map (cmd: lib.nameValuePair cmd "ask") (commands.ask.shell or [ ]);
          denies = map (cmd: lib.nameValuePair "${cmd} *" "deny") (commands.deny.shell or [ ])
            ++ map (cmd: lib.nameValuePair cmd "deny") (commands.deny.shell or [ ]);
        in
        lib.listToAttrs (allows ++ asks ++ denies)
        // { "*" = "ask"; };
    in
    {
      read = mkPathRules "read";
      glob = "allow";
      grep = "allow";
      list = "allow";
      bash = mkBashRules;
      edit = if tools.edit
        then (mkPathRules "edit")
        else (mkPathRules "edit" // { "*" = "deny"; });
      webfetch = "allow";
      websearch = "allow";
    };

  agentFiles = builtins.filter
    (name: lib.hasSuffix ".toml" name)
    (builtins.attrNames (builtins.readDir agentsDir));

  loadAgent = tomlFile:
    let
      name = lib.removeSuffix ".toml" tomlFile;
      agentConfig = builtins.fromTOML (builtins.readFile (agentsDir + "/${tomlFile}"));
      prompt = builtins.readFile (agentsDir + "/${name}.md");
      mode = agentConfig.mode or "plan";
    in
    {
      inherit name;
      value = agentConfig // { inherit prompt mode; };
    };

  agents = builtins.listToAttrs (map loadAgent agentFiles);

  # Filter agents that have opencode configuration
  opencodeAgents = lib.filterAttrs (name: agent: agent ? opencode) agents;

  validateOpencodeAgent = name: agent:
    let
      valid =
        if !(agent ? description) then throw "Agent ${name}: missing 'description'"
        else agent;
    in valid;

  validOpencodeAgents = lib.mapAttrs validateOpencodeAgent opencodeAgents;

  mkOpencodeAgent = name: agent:
    let
      isPrimary = (agent.opencode.primary or false);
      modelLine = lib.optionalString (isPrimary && agent.opencode ? model) "model: ${agent.opencode.model}\n";
      modeVal = if !isPrimary then "subagent" else (agent.opencode.mode or "primary");
      modeLine = lib.optionalString (modeVal != "") "mode: ${modeVal}\n";
      permissionLine = lib.optionalString (agent.opencode ? permission) "permission: ${builtins.toJSON agent.opencode.permission}\n";
    in
    ''
      ---
      description: ${agent.description}
      ${modelLine}${modeLine}${permissionLine}---
      ${agent.prompt}
    '';

  isStdioServer = server: server ? command || server ? package;

  toOpencodeMcpServers = servers:
    lib.mapAttrs
      (name: server:
        if isStdioServer server then {
          command = [ (server.command or (lib.getExe server.package)) ];
          type = "local";
          enabled = true;
        } else {
          url = server.url;
          type = "remote";
          enabled = true;
        })
      servers;
in
{
  programs.opencode = {
    enable = true;

    package = (pkgs.writeScriptBin "opencode" ''
      #!${pkgs.runtimeShell}
      exec "${lib.getExe pkgs.local.envWrapper}" \
        -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env" \
        -a GOOGLE_GENERATIVE_AI_API_KEY=GEMINI_API_KEY \
        -- "${lib.getExe pkgs.unstable.opencode}" "$@"
    '');

    agents = lib.mapAttrs mkOpencodeAgent validOpencodeAgents;
    rules = instructionText + ''

      ## Skill Execution (Subagent Enhancement)
      When executing a skill, if a `SUBAGENT.md` file exists alongside `SKILL.md` in the skill directory, read and follow `SUBAGENT.md` instead of `SKILL.md`. The subagent version uses specialized agents for higher-quality results.
    '';

    settings = {
      theme = "system";
      mcp = toOpencodeMcpServers config.programs.mcp.servers;
      mode = {
        plan.model = "fireworks-ai/accounts/fireworks/models/kimi-k2p5";
        build.model = "fireworks-ai/accounts/fireworks/models/kimi-k2p5";
      };
      permission = toOpencodePermissions "build";
    };
  };

  xdg.configFile."opencode/skill/home-manager".source = skillsDir;
}
