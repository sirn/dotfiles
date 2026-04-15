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
      mkToolPerm =
        tool: if cfg.sandbox.enabled && pkgs.stdenv.hostPlatform.isLinux then tool else "${tool}(**)";
      baseTools = [
        "Glob(*)"
        "Grep(*)"
        (mkToolPerm "Read")
        "WebSearch"
      ]
      ++ lib.optional tools.edit (mkToolPerm "Edit")
      ++ lib.optional tools.write (mkToolPerm "Write")
      ++ webFetchRules;

      pathAllows = lib.optionals (!cfg.sandbox.enabled) (
        map (p: "Read(${p})") (paths.allow.read or [ ])
        ++ lib.optionals tools.edit (map (p: "Edit(${p})") (paths.allow.edit or [ ]))
        ++ lib.optionals tools.write (map (p: "Write(${p})") (paths.allow.write or [ ]))
      );

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

      pathDenies = lib.optionals (!cfg.sandbox.enabled) (
        map (p: "Read(${p})") (paths.deny.read or [ ])
        ++ lib.optionals tools.edit (map (p: "Edit(${p})") (paths.deny.edit or [ ]))
        ++ lib.optionals tools.write (map (p: "Write(${p})") (paths.deny.write or [ ]))
      );

      bashDenies = mkBashPatterns (commands.deny or [ ]);
      deny = pathDenies ++ bashDenies;
    in
    {
      inherit allow ask deny;
    };

  claudeCodeAgents = lib.filterAttrs (name: agent: agent.claude-code != null) agentsCfg.subagents;

  validateClaudeCodeAgent =
    name: agent:
    let
      cc = agent.claude-code;
      valid =
        if !(agent ? description) then
          throw "Agent ${name}: missing 'description'"
        else if !(cc ? allowedTools) then
          throw "Agent ${name}: missing 'claude-code.allowedTools'"
        else if !(cc ? color) then
          throw "Agent ${name}: missing 'claude-code.color'"
        else if !(cc ? model) then
          throw "Agent ${name}: missing 'claude-code.model'"
        else
          agent;
    in
    valid;

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

  # Link individual skills rather than the entire directory,
  # allowing users to add custom skills alongside managed ones
  skillsDirContents = builtins.readDir agentsCfg.skillsDir;
  skillDirs = lib.filterAttrs (_: type: type == "directory") skillsDirContents;
  mkClaudeSkillLink = name: { ".claude/skills/${name}".source = agentsCfg.skillsDir + "/${name}"; };
  claudeSkillLinks = lib.foldl' (acc: name: acc // mkClaudeSkillLink name) { } (
    builtins.attrNames skillDirs
  );
in
{
  programs.claude-code = {
    enable = true;
    package = (
      pkgs.writeScriptBin "claude" ''
        #!${pkgs.runtimeShell}
        export DISABLE_AUTOUPDATER=1
        export DISABLE_INSTALLATION_CHECKS=1
        exec "${lib.getExe pkgs.local.envWrapper}" \
          -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env" \
          -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env.local" \
          -- "${lib.getExe pkgs.unstable.claude-code}" "$@"
      ''
    );

    agents = lib.mapAttrs mkClaudeCodeAgent validClaudeCodeAgents;
    memory.text = agentsCfg.instructionText + ''

      ## Skill Execution (Subagent Enhancement)

      When executing a skill, if a `SUBAGENT.md` file exists alongside `SKILL.md` in the skill directory, read and follow `SUBAGENT.md` instead of `SKILL.md`. The subagent version uses specialized agents via the Task tool for higher-quality results.
    '';
    mcpServers = toClaudeCodeMcpServers config.programs.mcp.servers;

    settings = {
      model = "opusplan";
      autoUpdaterStatus = "disabled";
      includeCoAuthoredBy = true;
      cleanupPeriodDays = 7;
      effortLevel = "medium";
      theme = config.home.colors.variant;
      statusLine = {
        type = "command";
        command = lib.getExe statusLineScript;
      };
      permissions = toClaudePermissions;
    };
  };

  programs.claude-code.sandbox.enabled = pkgs.stdenv.hostPlatform.isLinux;

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
    // lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux (
      let
        arch = if pkgs.stdenv.hostPlatform.isx86_64 then "x64" else "arm64";
        seccompDir = ".npm/lib/node_modules/@anthropic-ai/sandbox-runtime/vendor/seccomp/${arch}";
      in
      {
        # Workaround for Claude Code not respecting sandbox.seccomp.bpfPath and applyPath
        # https://github.com/anthropics/claude-code/issues/24238
        "${seccompDir}/apply-seccomp".source = "${pkgs.local.claude-code-seccomp}/bin/apply-seccomp";
        "${seccompDir}/unix-block.bpf".source =
          "${pkgs.local.claude-code-seccomp}/share/claude-code-seccomp/unix-block.bpf";
      }
    )
  );
}
