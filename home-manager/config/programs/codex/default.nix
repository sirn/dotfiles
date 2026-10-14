{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.codex;
  agentsCfg = config.agents;

  policy = agentsCfg.permissions.effective.build;

  # Render command entries into Starlark prefix_rule() statements for a given decision.
  # Codex prefix_rule only supports prefix matching; substring and args entries
  # cannot be represented and are skipped with a comment.
  mkRules =
    decision: entries:
    lib.concatMapStringsSep "\n" (
      entry:
      let
        m = entry.match;
        mode = entry.mode or "prefix";
        tokens = lib.splitString " " m;
        patternStr = "[${lib.concatMapStringsSep ", " (t: ''"${t}"'') tokens}]";
      in
      if mode == "substring" || mode == "args" then
        "# skipped (${mode} mode not supported by Codex prefix_rule): ${m}"
      else
        ''prefix_rule(pattern = ${patternStr}, decision = "${decision}")''
    ) entries;

  denyRules = mkRules "forbidden" (policy.commands.deny or [ ]);
  askRules = mkRules "prompt" (policy.commands.ask or [ ]);
  rulesContent = lib.concatStringsSep "\n\n" (
    lib.filter (s: s != "") [
      denyRules
      askRules
    ]
  );

  isStdioServer = server: server ? command || server ? package;

  toCodexMcpServers =
    servers:
    lib.mapAttrs (
      _name: server:
      (
        if isStdioServer server then
          { command = server.command or (lib.getExe server.package); }
        else
          { url = server.url; }
      )
      // lib.optionalAttrs (server ? allowedTools) { enabled_tools = server.allowedTools; }
      // lib.optionalAttrs (server ? disabled) { enabled = !server.disabled; }
    ) servers;

  # Convert nested attribute sets into `-c dotted.key=tomlValue` CLI flags so
  # declarative settings are applied without mutating `~/.codex/config.toml`.
  toConfigFlags =
    let
      flatten =
        prefix: value:
        if builtins.isAttrs value then
          lib.concatMap (
            k:
            let
              key = if builtins.match "^[A-Za-z0-9_-]+$" k != null then k else builtins.toJSON k;
              path = if prefix == "" then key else "${prefix}.${key}";
            in
            flatten path value.${k}
          ) (builtins.attrNames value)
        else
          [
            "-c"
            "${prefix}=${builtins.toJSON value}"
          ];
    in
    flatten "";

  # Link each rendered skill individually so ~/.codex/skills remains a writable
  # directory for runtime .system skills and unmanaged local skills.
  codexSkillLinks = lib.listToAttrs (
    map (skill: {
      name = ".codex/skills/${skill.name}";
      value.source = agentsCfg.skillTrees + "/${skill.name}";
    }) agentsCfg.discoveredSkills
  );

  wrappedCodex = config.agents.sandbox.mkWrapper {
    name = "codex";
    package = pkgs.llm-agents.codex;
    preExports = lib.optionalString pkgs.stdenv.hostPlatform.isLinux ''
      export PATH="${lib.makeBinPath [ pkgs.bubblewrap ]}:$PATH"
    '';
    extraArgs = [ "--no-daemon" ] ++ toConfigFlags cfg.settingsOverride;
  };
in
{
  programs.codex = {
    enable = true;
    package = wrappedCodex;
    context = agentsCfg.instructionText;
    rules.default = rulesContent;

    settingsOverride = {
      model_reasoning_effort = "high";
      check_for_update_on_startup = false;

      features = {
        daemon_auto_start = false;
        multi_agent = true;
      };

      tui = {
        notifications = true;
        status_line = [
          "model-with-reasoning"
          "git-branch"
          "context-used"
          "five-hour-limit"
        ];
      };

      sandbox_workspace_write = {
        network_access = true;
      };

      mcp_servers = toCodexMcpServers config.programs.mcp.servers;
    };
  };

  home.file = lib.mkIf cfg.enable codexSkillLinks;
}
