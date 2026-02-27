{ config, lib, pkgs, ... }:

let
  cfg = config.programs.claude-code;

  instructionText = builtins.readFile ../../../var/agents/instruction.md;

  skillsDir = ../../../var/agents/skills;

  agentsDir = ../../../var/agents/agents;

  permissionsPolicy = builtins.fromTOML (builtins.readFile ../../../var/agents/permissions.toml);
  domainsPolicy = builtins.fromTOML (builtins.readFile ../../../var/agents/domains.toml);

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

  toClaudePermissions = mode:
    let
      policy = effectivePolicy mode;
      inherit (policy) tools commands paths;

      webFetchRules = map (d: "WebFetch(domain:${d})") (domainsPolicy.allowed or [ ]);
      baseTools = [ "Glob(*)" "Grep(*)" "Read(**)" "WebSearch" ]
        ++ lib.optional tools.edit "Edit(**)"
        ++ lib.optional tools.write "Write(**)"
        ++ webFetchRules;

      pathAllows = map (p: "Read(${p})") (paths.allow.read or [ ])
        ++ lib.optionals tools.edit (map (p: "Edit(${p})") (paths.allow.edit or [ ]))
        ++ lib.optionals tools.write (map (p: "Write(${p})") (paths.allow.write or [ ]));

      mkBashPatterns = cmds: lib.concatMap (cmd: [ "Bash(${cmd})" "Bash(${cmd} *)" ]) cmds;
      bashAllows = mkBashPatterns (commands.allow.shell or [ ]);
      mcpAllows = claudeCodeMcpPermissions;
      allow = baseTools ++ pathAllows ++ bashAllows ++ mcpAllows;

      ask = mkBashPatterns (commands.ask.shell or [ ]);

      pathDenies = map (p: "Read(${p})") (paths.deny.read or [ ])
        ++ lib.optionals tools.edit (map (p: "Edit(${p})") (paths.deny.edit or [ ]))
        ++ lib.optionals tools.write (map (p: "Write(${p})") (paths.deny.write or [ ]));

      bashDenies = mkBashPatterns (commands.deny.shell or [ ]);
      deny = pathDenies ++ bashDenies;
    in
    { inherit allow ask deny; };

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

  # Filter agents that have claude-code configuration
  claudeCodeAgents = lib.filterAttrs (name: agent: agent ? claude-code) agents;

  validateClaudeCodeAgent = name: agent:
    let
      valid =
        if !(agent ? description) then throw "Agent ${name}: missing 'description'"
        else if !(agent.claude-code ? allowedTools) then throw "Agent ${name}: missing 'claude-code.allowedTools'"
        else if !(agent.claude-code ? color) then throw "Agent ${name}: missing 'claude-code.color'"
        else if !(agent.claude-code ? model) then throw "Agent ${name}: missing 'claude-code.model'"
        else agent;
    in valid;

  validClaudeCodeAgents = lib.mapAttrs validateClaudeCodeAgent claudeCodeAgents;

  mkClaudeCodeAgent = name: agent: ''
    ---
    name: ${name}
    description: ${agent.description}
    tools: ${lib.concatStringsSep ", " agent.claude-code.allowedTools}
    color: ${agent.claude-code.color}
    model: ${agent.claude-code.model}
    ---
    ${agent.prompt}
  '';

  isStdioServer = server: server ? command || server ? package;

  toClaudeCodeMcpServers = servers:
    lib.mapAttrs
      (name: server:
        if isStdioServer server then {
          type = "stdio";
          command = server.command or (lib.getExe server.package);
        } else {
          type = server.transport or "sse";
          url = server.url;
        })
      servers;

  # Generate MCP permissions from server allowedTools
  claudeCodeMcpPermissions = lib.flatten (lib.mapAttrsToList (name: server:
    let tools = server.allowedTools or null; in
    if tools == null
    then [ "mcp__${name}__*" ]
    else map (tool: "mcp__${name}__${tool}") tools
  ) config.programs.mcp.servers);

  statusLineScript = pkgs.writeShellApplication {
    name = "claude-statusline";
    runtimeInputs = [
      pkgs.jq
      pkgs.git
      pkgs.gawk
      config.programs.jujutsu.package
    ];
    text = builtins.readFile ./statusline.sh;
  };
in
{
  programs.claude-code = {
    enable = true;
    package = pkgs.unstable.claude-code;

    agents = lib.mapAttrs mkClaudeCodeAgent validClaudeCodeAgents;
    memory.text = instructionText + ''

      ## Skill Execution (Subagent Enhancement)
      When executing a skill, if a `SUBAGENT.md` file exists alongside `SKILL.md` in the skill directory, read and follow `SUBAGENT.md` instead of `SKILL.md`. The subagent version uses specialized agents via the Task tool for higher-quality results.
    '';
    mcpServers = toClaudeCodeMcpServers config.programs.mcp.servers;

    settings = {
      model = "opusplan";
      includeCoAuthoredBy = true;
      cleanupPeriodDays = 7;
      statusLine = {
        type = "command";
        command = lib.getExe statusLineScript;
      };
      permissions = toClaudePermissions "build";
    };
  };

  programs.git = lib.mkIf cfg.enable {
    ignores = [
      ".claude/*.local.json"
      ".claude/*.local.md"
    ];
  };

  home.file.".claude/skills".source = lib.mkIf cfg.enable skillsDir;
}
