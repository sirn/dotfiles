{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.pi-coding-agent;

  skillsDir = ../../../var/agents/skills;
  instructionText = builtins.readFile ../../../var/agents/instruction.md;
  permissionsToml = lib.importTOML ../../../var/agents/permissions.toml;
  modelsData = builtins.fromTOML (builtins.readFile ../../../var/agents/models.toml);

  agentPermissionsPath = ../../../var/agents/permissions.pi.toml;
  agentPermissions =
    if builtins.pathExists agentPermissionsPath then lib.importTOML agentPermissionsPath else { };

  # Transform TOML model to Pi format (snake_case -> camelCase)
  toPiModel = m: {
    id = m.id;
    name = m.name;
    reasoning = m.reasoning;
    input = m.input;
    contextWindow = m.context_window;
    maxTokens = m.max_tokens;
    cost = {
      input = m.cost_input;
      output = m.cost_output;
      cacheRead = m.cost_cache_read;
      cacheWrite = m.cost_cache_write;
    };
  };

  # Build provider config from models.toml
  mkPiProvider = name: p: {
    baseUrl = p.base_url;
    apiKey = p.env_var;
    api = p.api;
    defaultThinkingLevel = p.reasoning_effort;
    models = map toPiModel p.models;
  };

  wrappedPi = pkgs.writeScriptBin "pi" ''
    #!${pkgs.runtimeShell}
    exec "${lib.getExe pkgs.local.envWrapper}" \
      -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env" \
      -- "${lib.getExe pkgs.local.pi-coding-agent-bin}" "$@"
  '';

  agentsMdText = ''
    ${instructionText}
    ## Safety Guidelines (Pi-specific)

    - When running destructive commands (`rm`, etc.), you must first ask the user.
    - When doing a commit, ask user for confirmation first.
    - Do not squash commit unless being told explicitly by the user.
  '';

  # Generate unified policy JSON for all extensions
  policyJson = builtins.toJSON {
    default = {
      commands = {
        allow =
          permissionsToml.default.commands.allow.shell
          ++ ((agentPermissions.default or { }).commands.allow.shell or [ ]);
        ask =
          permissionsToml.default.commands.ask.shell
          ++ ((agentPermissions.default or { }).commands.ask.shell or [ ]);
        deny =
          permissionsToml.default.commands.deny.shell
          ++ ((agentPermissions.default or { }).commands.deny.shell or [ ]);
      };
      wrappers = (permissionsToml.default.commands.wrappers or [ ]);
      redirects = permissionsToml.default.redirects or { action = "allow"; };
      heredocs = permissionsToml.default.heredocs or { action = "ask"; };
    };
    modes.plan = {
      tools =
        permissionsToml.mode.plan.tools or {
          edit = false;
          write = false;
        };
      commands = {
        deny = (permissionsToml.mode.plan.commands or { }).deny.shell or [ ];
        ask = (permissionsToml.mode.plan.commands or { }).ask.shell or [ ];
        allow = (permissionsToml.mode.plan.commands or { }).allow.shell or [ ];
      };
      wrappers =
        (permissionsToml.default.commands.wrappers or [ ])
        ++ (permissionsToml.mode.plan.commands.wrappers or [ ]);
      redirects =
        permissionsToml.mode.plan.redirects or {
          action = "deny";
          safeTargets = [
            "/dev/null"
            "/dev/stderr"
            "/dev/stdout"
          ];
          allowFdDup = true;
        };
      heredocs = permissionsToml.mode.plan.heredocs or { action = "ask"; };
    };
  };

  # Write JSON file to store path (safer than echo in shell)
  policyJsonFile = pkgs.writeTextFile {
    name = "policy.json";
    text = policyJson;
  };

  # Combine bundled extensions with generated JSON config into a single derivation
  bundledAgent = pkgs.runCommand "pi-bundled-agent" { } ''
    mkdir -p $out/extensions/home-manager
    cp -r ${./extensions}/. $out/extensions/home-manager/
    cp ${policyJsonFile} $out/policy.json
  '';

in
{
  programs.pi-coding-agent = {
    enable = true;

    package = wrappedPi;

    instructionText = agentsMdText;

    settings = {
      quietStartup = true;
      defaultProvider = "fireworks-ai";
      defaultModel = "accounts/fireworks/routers/kimi-k2p5-turbo";
      defaultThinkingLevel = "high";
      hideThinkingBlock = false;
      enabledModels = [
        # Fireworks
        "accounts/fireworks/models/*"
        "accounts/fireworks/routers/*"

        # Synthetic
        "hf:zai-org/*"
        "hf:moonshotai/*"
        "hf:MiniMaxAI/*"

        # Anthropic Claude
        "claude-opus-4-6"
        "claude-sonnet-4-6"

        # OpenAI
        "gpt-5.4"
        "gpt-5.4-mini"
        "gpt-5.4-nano"

        # Google Gemini
        "gemini-3.1-pro-preview"
        "gemini-3.1-flash-lite-preview"
      ];
      retry = {
        maxRetries = 10;
        maxDelayMs = 0;
      };
    };

    providers = lib.mkForce (lib.mapAttrs mkPiProvider modelsData.providers);

    keybindings = {
      # Cursor Movement (Emacs)
      "tui.editor.cursorUp" = [
        "up"
        "ctrl+p"
      ];
      "tui.editor.cursorDown" = [
        "down"
        "ctrl+n"
      ];
      "tui.editor.cursorLeft" = [
        "left"
        "ctrl+b"
      ];
      "tui.editor.cursorRight" = [
        "right"
        "ctrl+f"
      ];
      "tui.editor.cursorWordLeft" = [
        "alt+left"
        "ctrl+left"
        "alt+b"
      ];
      "tui.editor.cursorWordRight" = [
        "alt+right"
        "ctrl+right"
        "alt+f"
      ];
      "tui.editor.cursorLineStart" = [
        "home"
        "ctrl+a"
      ];
      "tui.editor.cursorLineEnd" = [
        "end"
        "ctrl+e"
      ];

      # Deletion (Emacs)
      "tui.editor.deleteCharBackward" = [
        "backspace"
        "ctrl+h"
      ];
      "tui.editor.deleteCharForward" = [
        "delete"
        "ctrl+d"
      ];
      "tui.editor.deleteWordBackward" = [
        "ctrl+w"
        "alt+backspace"
      ];
      "tui.editor.deleteWordForward" = [
        "alt+d"
        "alt+delete"
      ];
      "tui.editor.deleteToLineStart" = [ "ctrl+u" ];
      "tui.editor.deleteToLineEnd" = [ "ctrl+k" ];

      # Text Input
      "tui.input.newLine" = [
        "shift+enter"
        "ctrl+j"
      ];
      "tui.input.submit" = [ "enter" ];
      "tui.input.tab" = [ "tab" ];

      # Selection (for ctx.ui.select dialogs)
      "tui.select.up" = [
        "up"
        "ctrl+p"
      ];
      "tui.select.down" = [
        "down"
        "ctrl+n"
      ];
      "tui.select.confirm" = [ "enter" ];
      "tui.select.cancel" = [
        "escape"
        "ctrl+c"
      ];

      # Tree Navigation (session tree view)
      "app.tree.foldOrUp" = [
        "ctrl+left"
        "alt+left"
      ];
      "app.tree.unfoldOrDown" = [
        "ctrl+right"
        "alt+right"
      ];

      # Kill Ring (Emacs)
      "tui.editor.yank" = [ "ctrl+y" ];
      "tui.editor.yankPop" = [ "alt+y" ];
      "tui.editor.undo" = [
        "ctrl+_"
        "ctrl+/"
      ];

      # Application
      "app.interrupt" = [ "escape" ];
      "app.clear" = [ "ctrl+c" ];
      "app.exit" = [ "ctrl+d" ];
      "app.editor.external" = [ "ctrl+g" ];

      # Models and Thinking
      "app.model.select" = [ "ctrl+l" ];
      "app.model.cycleForward" = [ "ctrl+period" ];
      "app.model.cycleBackward" = [ "ctrl+comma" ];
      "app.thinking.cycle" = [ "shift+tab" ];

      # Display
      "app.tools.expand" = [ "ctrl+o" ];
      "app.thinking.toggle" = [ "ctrl+t" ];

      # Message Queue
      "app.message.followUp" = [ "alt+enter" ];
      "app.message.dequeue" = [ "alt+up" ];
    };
  };

  home.file = {
    ".pi/agent/skills/home-manager".source = skillsDir;
    ".pi/agent/extensions/home-manager".source = "${bundledAgent}/extensions/home-manager";
    ".pi/agent/policy.json".source = "${bundledAgent}/policy.json";
  };
}
