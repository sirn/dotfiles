{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.pi-coding-agent;
  agentsCfg = config.agents;

  # Transform module model to Pi format
  toPiModel =
    m:
    {
      id = m.id;
      name = m.name;
      reasoning = m.reasoning;
      input = m.input;
      contextWindow = m.contextWindow;
      maxTokens = m.maxTokens;
      cost = {
        input = m.costInput;
        output = m.costOutput;
        cacheRead = m.costCacheRead;
        cacheWrite = m.costCacheWrite;
      };
    }
    // lib.optionalAttrs (m.api != null) { api = m.api; };

  # Build provider config from agents.models
  mkPiProvider =
    name: p:
    {
      baseUrl = p.baseUrl;
      apiKey = p.envVar;
      api = p.api;
      defaultThinkingLevel = p.reasoningEffort;
      models = map toPiModel p.models;
    }
    // lib.optionalAttrs (!p.compatibility.developerRole) { compat.supportsDeveloperRole = false; };

  wrappedPi = pkgs.writeScriptBin "pi" ''
    #!${pkgs.runtimeShell}
    exec "${lib.getExe pkgs.local.envWrapper}" \
      -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env" \
      -- "${lib.getExe pkgs.unstable.pi-coding-agent}" "$@"
  '';

  agentsMdText = ''
    ${agentsCfg.instructionText}
    ## Safety Guidelines (Pi-specific)

    - When running destructive commands (`rm`, etc.), you must first ask the user.
    - When doing a commit, ask user for confirmation first.
    - Do not squash commit unless being told explicitly by the user.
  '';

  perms = agentsCfg.permissions;

  # Generate unified policy JSON for all extensions
  policyJson = builtins.toJSON {
    default = {
      commands = {
        allow = perms.default.commands.allow;
        ask = perms.default.commands.ask;
        deny = perms.default.commands.deny;
      };
      wrappers = perms.default.wrappers;
      redirects = perms.default.redirects;
      heredocs = perms.default.heredocs;
    };
    modes.plan = {
      tools = perms.modes.plan.tools;
      commands = {
        deny = perms.modes.plan.commands.deny;
        ask = perms.modes.plan.commands.ask;
        allow = perms.modes.plan.commands.allow;
      };
      wrappers = perms.default.wrappers ++ perms.modes.plan.wrappers;
      redirects = perms.modes.plan.redirects;
      heredocs = perms.modes.plan.heredocs;
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
      defaultProvider = agentsCfg.models.default.provider;
      defaultModel = agentsCfg.models.default.model;
      defaultThinkingLevel = "high";
      hideThinkingBlock = false;
      enabledModels = lib.concatMap (p: map (m: m.id) p.models) (
        builtins.attrValues agentsCfg.models.providers
      );
      retry = {
        maxRetries = 10;
        maxDelayMs = 0;
      };
    };

    providers = lib.mapAttrs mkPiProvider agentsCfg.models.providers;

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
    ".pi/agent/skills/home-manager".source = agentsCfg.skillsDir;
    ".pi/agent/extensions/home-manager".source = "${bundledAgent}/extensions/home-manager";
    ".pi/agent/policy.json".source = "${bundledAgent}/policy.json";
  };
}
