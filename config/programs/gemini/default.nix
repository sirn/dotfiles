{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.programs.gemini-cli;

  instructionText = builtins.readFile ../../../var/agents/instruction.md;

  skillsDir = ../../../var/agents/skills;

  permissionsPolicy = builtins.fromTOML (builtins.readFile ../../../var/agents/permissions.toml);

  effectivePolicy =
    mode:
    let
      default = permissionsPolicy.default or { };
      modePolicy = permissionsPolicy.mode.${mode} or { };
      mergeLists = lists: lib.unique (lib.concatLists (lib.filter (l: l != null) lists));
      mergeCmds =
        section:
        let
          defaultShell = (default.commands.${section} or { }).shell or [ ];
          modeShell = ((modePolicy.commands or { }).${section} or { }).shell or [ ];
        in
        {
          shell = mergeLists [
            defaultShell
            modeShell
          ];
        };
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

  tomlFormat = pkgs.formats.toml { };

  toGeminiPolicyRules =
    mode:
    let
      policy = effectivePolicy mode;
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
            { commandRegex = "\\b${escapeRegex entry.match}\\b"; }
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
    ++ (map (mkShellRule "allow" 150) (commands.allow.shell or [ ]))
    ++ (map (mkShellRule "ask_user" 150) (commands.ask.shell or [ ]))
    ++ (map (mkShellRule "deny" 150) (commands.deny.shell or [ ]))
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

  policyRules = toGeminiPolicyRules "build";
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
            {
              command = server.command or (lib.getExe server.package);
            }
          else
            {
              command = lib.getExe pkgs.local.mcpServers.mcp-remote;
              args = [ server.url ];
            };
        # Add trust setting based on allowedTools
        trustConfig = if tools == null then { trust = true; } else { };
      in
      baseConfig // trustConfig
    ) servers;

  # Link individual skills rather than the entire directory,
  # allowing users to add custom skills alongside managed ones
  skillsDirContents = builtins.readDir skillsDir;
  skillDirs = lib.filterAttrs (_: type: type == "directory") skillsDirContents;
  mkGeminiSkillLink = name: {
    ".gemini/skills/${name}".source = skillsDir + "/${name}";
  };
  geminiSkillLinks = lib.foldl' (acc: name: acc // mkGeminiSkillLink name) { } (
    builtins.attrNames skillDirs
  );
in
{
  programs.gemini-cli = {
    enable = true;

    package = (
      pkgs.writeScriptBin "gemini" ''
        #!${pkgs.runtimeShell}
        exec "${lib.getExe pkgs.local.envWrapper}" \
          -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env" \
          -- "${lib.getExe pkgs.local.gemini-cli-bin}" "$@"
      ''
    );

    # In 25.11, defaultModel only accepts string and default to gemini-2.5-pro
    # We want to use the best Auto model; so this needs to be set to an empty string.
    #
    # TODO: switch to null, >25.11
    defaultModel = "";

    context.AGENTS = instructionText;

    settings = {
      mcpServers = toGeminiMcpServers config.programs.mcp.servers;
      context.fileName = [
        "AGENTS.md"
        "GEMINI.md"
      ];
      general = {
        enablePromptCompletion = true;
        previewFeatures = true;
        sessionRetention = {
          enabled = true;
          maxAge = "7d";
          maxCount = 100;
        };
      };
      ui = {
        theme = "ANSI";
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
                  thinkingBudget = 24576;
                  includeThoughts = true;
                };
              };
            };
          }
        ];
      };
    };
  };

  programs.git = lib.mkIf cfg.enable {
    ignores = [
      ".gemini/"
    ];
  };

  home.file = lib.mkIf cfg.enable (
    geminiSkillLinks
    // {
      ".gemini/policies/nix-managed.toml".source = policyFile;
    }
  );
}
