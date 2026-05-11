{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.programs.gemini-cli;
  agentsCfg = config.agents;

  policy = agentsCfg.permissions.effective.build;

  tomlFormat = pkgs.formats.toml { };

  toGeminiPolicyRules =
    let
      inherit (policy) tools commands paths;

      globToRegex =
        glob:
        let
          r1 = lib.replaceStrings [ "." ] [ "\\." ] glob;
          p1 = lib.replaceStrings [ "**" ] [ "DOUBLESTAR" ] r1;
          p2 = lib.replaceStrings [ "*" ] [ "[^/]*" ] p1;
          p3 = lib.replaceStrings [ "DOUBLESTAR" ] [ ".*" ] p2;
        in
        p3;

      escapeRegex =
        str:
        builtins.replaceStrings
          [
            "."
            "*"
            "+"
            "?"
            "^"
            "$"
            "{"
            "}"
            "("
            ")"
            "|"
            "["
            "]"
            "\\"
          ]
          [
            "\\."
            "\\*"
            "\\+"
            "\\?"
            "\\^"
            "\\$"
            "\\{"
            "\\}"
            "\\("
            "\\)"
            "\\|"
            "\\["
            "\\]"
            "\\\\"
          ]
          str;

      mkShellRule =
        decision: priority: entry:
        {
          toolName = "run_shell_command";
          decision = decision;
          priority = priority;
        }
        // (
          if entry.mode == "exact" then
            { commandRegex = "^${escapeRegex entry.match}$"; }
          else if entry.mode == "substring" then
            { commandRegex = "(?:^|\\s)${escapeRegex entry.match}(?:\\s|$)"; }
          else if entry.mode == "args" then
            let
              parts = lib.splitString ":" entry.match;
              program = builtins.head parts;
              argsStr = lib.concatStringsSep ":" (builtins.tail parts);
              argParts = lib.filter (s: s != "") (lib.splitString " " argsStr);
              programPrefix = if program == "*" then "" else "^${escapeRegex program}\\s";
              argLookaheads = lib.concatMapStrings (a: "(?=.*(?:^|\\s)${escapeRegex a}(?:\\s|$))") argParts;
            in
            {
              commandRegex = "${programPrefix}${argLookaheads}";
            }
          else
            { commandPrefix = entry.match; }
        );

      mkPathRule = decision: priority: tool: glob: {
        toolName = tool;
        argsPattern = "file_path\":\"${globToRegex glob}";
        decision = decision;
        priority = priority;
      };

      baseTools =
        lib.optional tools.read "read_file"
        ++ lib.optional tools.glob "glob"
        ++ lib.optional tools.grep "grep_search"
        ++ lib.optional tools.list "list_directory"
        ++ lib.optional tools.webfetch "web_fetch"
        ++ lib.optional tools.websearch "google_web_search"
        ++ [
          "ask_user"
          "activate_skill"
        ];
    in
    [
      {
        toolName = baseTools;
        decision = "allow";
        priority = 100;
      }
    ]
    ++ lib.optional tools.edit {
      toolName = [ "replace" ];
      decision = "allow";
      priority = 100;
    }
    ++ lib.optional tools.write {
      toolName = [ "write_file" ];
      decision = "allow";
      priority = 100;
    }
    ++ (map (mkShellRule "allow" 150) (commands.allow or [ ]))
    ++ (map (mkShellRule "ask_user" 150) (commands.ask or [ ]))
    ++ (map (mkShellRule "deny" 150) (commands.deny or [ ]))
    # We map path rules specifically for each tool
    ++ (lib.concatMap (glob: [
      (mkPathRule "deny" 200 "read_file" glob)
      (mkPathRule "deny" 200 "replace" glob)
      (mkPathRule "deny" 200 "write_file" glob)
    ]) (paths.deny.read or [ ]))
    ++ (lib.concatMap (glob: [
      (mkPathRule "allow" 250 "read_file" glob)
      (mkPathRule "allow" 250 "replace" glob)
      (mkPathRule "allow" 250 "write_file" glob)
    ]) (paths.allow.read or [ ]));

  policyRules = toGeminiPolicyRules;
  policyFile = tomlFormat.generate "gemini-policy.toml" { rule = policyRules; };

  isStdioServer = server: server ? command || server ? package;

  toGeminiMcpServers =
    servers:
    lib.mapAttrs (
      name: server:
      let
        tools = server.allowedTools or null;
        baseConfig =
          if isStdioServer server then
            { command = server.command or (lib.getExe server.package); }
          else
            { url = server.url; };
        # Add trust setting based on allowedTools
        trustConfig = if tools == null then { trust = true; } else { };
      in
      baseConfig // trustConfig
    ) servers;

  # Gemini only discovers immediate children of ~/.gemini/skills.
  # Link each rendered skill individually so grouped vendored skill sets are flattened
  # into the shape Gemini expects while still allowing unmanaged local skills.
  geminiSkillLinks = lib.listToAttrs (
    map (skill: {
      name = ".gemini/skills/${skill.name}";
      value.source = agentsCfg.skillTrees.default + "/${skill.name}";
    }) agentsCfg.discoveredSkills
  );

  # Generate gemini-compatible agent markdown files from subagent configs
  geminiAgentFiles = builtins.listToAttrs (
    builtins.filter (a: a != null) (
      builtins.attrValues (
        builtins.mapAttrs (
          name: agentCfg:
          if agentCfg.gemini != null then
            let
              geminiCfg = agentCfg.gemini;
              tools = lib.concatMapStrings (t: "\n  - \"${t}\"") geminiCfg.tools;
              content = ''
                ---
                name: ${name}
                description: ${agentCfg.description}
                kind: local
                tools:${tools}
                model: ${geminiCfg.model}
                ---
                ${agentCfg.prompt}'';
            in
            {
              name = "${name}.md";
              value = pkgs.writeText "${name}.md" content;
            }
          else
            null
        ) agentsCfg.subagents
      )
    )
  );

  geminiAgentsDir = pkgs.runCommand "gemini-agents" { } ''
    mkdir -p $out
    ${lib.concatStringsSep "\n" (
      lib.mapAttrsToList (name: path: "cp ${path} $out/${name}") geminiAgentFiles
    )}
  '';
in
{
  programs.gemini-cli = {
    enable = true;

    package = (
      pkgs.writeScriptBin "gemini" ''
        #!${pkgs.runtimeShell}
        exec "${lib.getExe pkgs.local.envWrapper}" \
          -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env" \
          -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env.local" \
          -- "${lib.getExe pkgs.unstable.gemini-cli}" "$@"
      ''
    );

    # In 25.11, defaultModel only accepts string and default to gemini-2.5-pro
    # We want to use the best Auto model; so this needs to be set to an empty string.
    #
    # TODO: switch to null, >25.11
    defaultModel = "";

    context.AGENTS = agentsCfg.instructionText;

    settings = {
      mcpServers = toGeminiMcpServers config.programs.mcp.servers;
      context.fileName = [
        "AGENTS.md"
        "GEMINI.md"
      ];
      general = {
        enableNotifications = true;
        enablePromptCompletion = true;
        previewFeatures = true;
        sessionRetention = {
          enabled = true;
          maxAge = "7d";
          maxCount = 100;
        };
      };
      ui = {
        autoThemeSwitching = true;
        inlineThinkingMode = "full";
      };
      security = {
        auth = {
          # Valid Types:
          # oauth-personal: Login with Google
          # gemini-api-key: Gemini API key
          # vertex-ai: Vertex AI API key
          # compute-default-credentials: Google Cloud default credentials
          #
          # We set this to oauth-personal by default to use our Google subscription.
          selectedType = lib.mkDefault "oauth-personal";
        };
        disableYoloMode = true;
      };
      tools = {
        autoAccept = true;
        sandbox = pkgs.stdenv.isDarwin;
      };
      modelConfigs = {
        overrides = [
          {
            match = { };
            modelConfig = {
              generateContentConfig = {
                thinkingConfig = {
                  thinkingLevel = "HIGH";
                  includeThoughts = true;
                };
              };
            };
          }
        ];
      };
    };
  };

  programs.git = lib.mkIf cfg.enable { ignores = [ ".gemini/" ]; };

  home.file = lib.mkIf cfg.enable (
    geminiSkillLinks
    // {
      ".gemini/agents".source = geminiAgentsDir;
      ".gemini/policies/nix-managed.toml".source = policyFile;
    }
  );
}
