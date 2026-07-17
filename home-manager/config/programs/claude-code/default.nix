{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.claude-code;
  agentsCfg = config.agents;

  policy = agentsCfg.permissions.effective.build;

  toClaudePermissions =
    let
      inherit (policy) tools commands paths;

      webFetchRules = map (d: "WebFetch(domain:${d})") agentsCfg.domains.allowed;
      mkToolPerm = tool: "${tool}(**)";
      baseTools = [
        "Glob(*)"
        "Grep(*)"
        (mkToolPerm "Read")
        "WebSearch"
      ]
      ++ lib.optional tools.edit (mkToolPerm "Edit")
      ++ lib.optional tools.write (mkToolPerm "Write")
      ++ webFetchRules;

      pathAllows =
        map (p: "Read(${p})") (paths.allow.read or [ ])
        ++ lib.optionals tools.edit (map (p: "Edit(${p})") (paths.allow.edit or [ ]))
        ++ lib.optionals tools.write (map (p: "Write(${p})") (paths.allow.write or [ ]));

      mkBashPatterns =
        cmds:
        lib.concatMap (
          entry:
          let
            m = entry.match;
          in
          {
            exact = [ "Bash(${m})" ];
            prefix = [
              "Bash(${m})"
              "Bash(${m} *)"
            ];
            substring = [
              "Bash(* ${m} *)"
              "Bash(${m} *)"
              "Bash(* ${m})"
            ];
            args =
              let
                parts = lib.splitString ":" m;
                argsStr = lib.concatStringsSep ":" (builtins.tail parts);
              in
              [
                "Bash(* ${argsStr} *)"
                "Bash(${argsStr} *)"
                "Bash(* ${argsStr})"
              ];
          }
          .${entry.mode or "prefix"}
        ) cmds;
      bashAllows = mkBashPatterns (commands.allow or [ ]);
      mcpAllows = claudeCodeMcpPermissions;
      allow = baseTools ++ pathAllows ++ bashAllows ++ mcpAllows;

      ask = mkBashPatterns (commands.ask or [ ]);

      pathDenies =
        map (p: "Read(${p})") (paths.deny.read or [ ])
        ++ lib.optionals tools.edit (map (p: "Edit(${p})") (paths.deny.edit or [ ]))
        ++ lib.optionals tools.write (map (p: "Write(${p})") (paths.deny.write or [ ]));

      bashDenies = mkBashPatterns (commands.deny or [ ]);
      deny = pathDenies ++ bashDenies;
    in
    {
      inherit allow ask deny;
      defaultMode = "auto";
    };

  isStdioServer = server: server ? command || server ? package;

  toClaudeCodeMcpServers =
    servers:
    lib.mapAttrs (
      name: server:
      if isStdioServer server then
        {
          type = "stdio";
          command = server.command or (lib.getExe server.package);
        }
      else
        {
          type = server.transport or "sse";
          url = server.url;
        }
    ) servers;

  # Generate MCP permissions from server allowedTools
  claudeCodeMcpPermissions = lib.flatten (
    lib.mapAttrsToList (
      name: server:
      let
        tools = server.allowedTools or null;
      in
      if tools == null then [ "mcp__${name}__*" ] else map (tool: "mcp__${name}__${tool}") tools
    ) config.programs.mcp.servers
  );

  statusLineScript = pkgs.writeShellApplication {
    name = "claude-statusline";
    runtimeInputs = [
      pkgs.jaq
      pkgs.git
      pkgs.gawk
      config.programs.jujutsu.package
    ];
    text = builtins.readFile ./statusline.sh;
  };

  # Claude Code only discovers immediate children of ~/.claude/skills.
  # Link each rendered skill individually so grouped vendored skill sets are flattened
  # into the shape Claude expects while still allowing unmanaged local skills.
  claudeSkillLinks = lib.listToAttrs (
    map (skill: {
      name = ".claude/skills/${skill.name}";
      value.source = agentsCfg.skillTrees + "/${skill.name}";
    }) agentsCfg.discoveredSkills
  );

  wrappedClaude = config.agents.sandbox.mkWrapper {
    name = "claude";
    package = pkgs.llm-agents.claude-code;
    preExports = ''
      export DISABLE_AUTOUPDATER=1
      export DISABLE_INSTALLATION_CHECKS=1
    '';
  };
in
{
  programs.claude-code = {
    enable = true;
    package = wrappedClaude;

    context = agentsCfg.instructionText;
    mcpServers = toClaudeCodeMcpServers config.programs.mcp.servers;

    settings = {
      autoUpdaterStatus = "disabled";
      autoMemoryEnabled = false;
      includeCoAuthoredBy = true;
      showClearContextOnPlanAccept = true;
      skipAutoPermissionPrompt = true;
      cleanupPeriodDays = 7;
      effortLevel = lib.mkDefault "high";
      tui = "fullscreen";

      statusLine = {
        type = "command";
        command = lib.getExe statusLineScript;
      };
      permissions = toClaudePermissions;
    };
  };

  programs.git = lib.mkIf cfg.enable {
    ignores = [
      ".claude/*.local.json"
      ".claude/*.local.md"
    ];
  };

  home.file = lib.mkIf cfg.enable (
    {
      ".claude/keybindings.json".text = builtins.toJSON {
        "$schema" = "https://www.schemastore.org/claude-code-keybindings.json";
        "$docs" = "https://code.claude.com/docs/en/keybindings";
        bindings = [
          {
            context = "Chat";
            bindings = {
              "ctrl+j" = "chat:newline";
            };
          }
        ];
      };
    }
    // claudeSkillLinks
  );
}
