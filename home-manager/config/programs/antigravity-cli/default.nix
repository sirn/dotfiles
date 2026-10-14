{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.antigravity-cli;
  agentsCfg = config.agents;

  jsonFormat = pkgs.formats.json { };
  overrideFile = jsonFormat.generate "antigravity-cli-settings-override.json" cfg.settingsOverride;

  policy = agentsCfg.permissions.effective.build;

  toAntigravityPermissions =
    let
      inherit (policy) tools commands paths;

      readUrlRules = map (d: "read_url(${d})") agentsCfg.domains.allowed;

      mkWritePerms =
        section:
        lib.unique (
          (lib.optionals tools.edit (map (p: "write_file(${p})") (paths.${section}.edit or [ ])))
          ++ (lib.optionals tools.write (map (p: "write_file(${p})") (paths.${section}.write or [ ])))
        );

      pathAllows = map (p: "read_file(${p})") (paths.allow.read or [ ]) ++ mkWritePerms "allow";
      pathDenies = map (p: "read_file(${p})") (paths.deny.read or [ ]) ++ mkWritePerms "deny";

      mkCommandPatterns =
        cmds:
        lib.concatMap (
          entry:
          let
            m = entry.match;
            escaped = lib.escapeRegex m;
          in
          {
            exact = [ "command(${m})" ];
            prefix = [ "command(${m})" ];
            substring = [
              "command(regex:.* ${escaped} .*)"
              "command(regex:${escaped} .*)"
              "command(regex:.* ${escaped})"
            ];
            args =
              let
                parts = lib.splitString ":" m;
                program = builtins.head parts;
                argsStr = lib.escapeRegex (lib.concatStringsSep ":" (builtins.tail parts));
                progPrefix = if program == "*" then ".*" else lib.escapeRegex program;
              in
              if program == "*" then
                [
                  "command(regex:.* ${argsStr} .*)"
                  "command(regex:${argsStr} .*)"
                  "command(regex:.* ${argsStr})"
                ]
              else
                [
                  "command(regex:${progPrefix} .* ${argsStr} .*)"
                  "command(regex:${progPrefix} ${argsStr} .*)"
                  "command(regex:${progPrefix} .* ${argsStr})"
                  "command(regex:${progPrefix} ${argsStr})"
                ];
          }
          .${entry.mode or "prefix"}
        ) cmds;

      cmdAllows = mkCommandPatterns (commands.allow or [ ]);
      cmdAsks = mkCommandPatterns (commands.ask or [ ]);
      cmdDenies = mkCommandPatterns (commands.deny or [ ]);
    in
    {
      allow = readUrlRules ++ pathAllows ++ cmdAllows ++ antigravityMcpPermissions;
      ask = cmdAsks;
      deny = pathDenies ++ cmdDenies;
    };

  isStdioServer = server: server ? command || server ? package;

  toAntigravityMcpServers =
    servers:
    lib.mapAttrs (
      _name: server:
      if isStdioServer server then
        { command = server.command or (lib.getExe server.package); }
      else
        { serverUrl = server.url; }
    ) servers;

  antigravityMcpPermissions = lib.flatten (
    lib.mapAttrsToList (
      name: server:
      let
        tools = server.allowedTools or null;
      in
      if tools == null then [ "mcp(${name}/*)" ] else map (tool: "mcp(${name}/${tool})") tools
    ) config.programs.mcp.servers
  );

  statusLineScript = pkgs.writeShellApplication {
    name = "antigravity-statusline";
    runtimeInputs = [
      pkgs.jaq
      pkgs.git
      pkgs.gawk
      config.programs.jujutsu.package
    ];
    text = builtins.readFile ../claude-code/statusline.sh;
  };

  # Link each rendered skill individually so grouped vendored skill sets are flattened
  # into ~/.gemini/config/skills while still allowing unmanaged local skills.
  antigravitySkillLinks = lib.listToAttrs (
    map (skill: {
      name = ".gemini/config/skills/${skill.name}";
      value.source = agentsCfg.skillTrees + "/${skill.name}";
    }) agentsCfg.discoveredSkills
  );

  wrappedAntigravity = config.agents.sandbox.mkWrapper {
    name = "agy";
    package = pkgs.llm-agents.antigravity-cli;
  };
in
{
  programs.antigravity-cli = {
    enable = true;
    package = wrappedAntigravity;

    context.AGENTS = agentsCfg.instructionText;
    mcpServers = toAntigravityMcpServers config.programs.mcp.servers;

    settingsOverride = {
      notifications = true;
      permissions = toAntigravityPermissions;
      statusLine = {
        type = "command";
        command = lib.getExe statusLineScript;
      };
    };
  };

  programs.git = lib.mkIf cfg.enable {
    ignores = [
      ".agent/"
      ".agents/"
      ".gemini/"
    ];
  };

  home.file = lib.mkIf cfg.enable antigravitySkillLinks;

  home.activation.mergeAntigravityConfig = lib.mkIf (cfg.enable && cfg.settingsOverride != { }) (
    lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      config="$HOME/.gemini/antigravity-cli/settings.json"
      mkdir -p "$(dirname "$config")"
      if [ -L "$config" ]; then
        rm "$config"
      fi
      if [ -s "$config" ]; then
        merged="$(${lib.getExe pkgs.jaq} --slurpfile override ${overrideFile} '(. // {}) * $override[0]' "$config")"
        printf '%s\n' "$merged" > "$config"
      else
        install -m 600 ${overrideFile} "$config"
      fi
    ''
  );
}
