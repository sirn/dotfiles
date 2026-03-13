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

  # Generate JSON config for safety-gate extension
  safetyGateJson = builtins.toJSON {
    allow = permissionsToml.default.commands.allow.shell;
    ask = permissionsToml.default.commands.ask.shell;
    deny = permissionsToml.default.commands.deny.shell;
  };

  # Load static TypeScript extension
  safetyGateTs = builtins.readFile ./safety-gate.ts;
in
{
  programs.pi-coding-agent = {
    enable = true;

    package = wrappedPi;

    instructionText = agentsMdText;

    extensions = [ "extensions/safety-gate.ts" ];

    settings = {
      quietStartup = true;
      defaultProvider = "synthetic";
      defaultModel = "hf:moonshotai/Kimi-K2.5";
      defaultThinkingLevel = "high";
      hideThinkingBlock = false;
      enabledModels = [
        # Synthetic/Fireworks
        "accounts/fireworks/models/*"
        "hf:zai-org/*"
        "hf:moonshotai/*"
        "hf:MiniMaxAI/*"

        # Anthropic Claude
        "claude-opus-4-6"
        "claude-sonnet-4-6"

        # OpenAI
        "gpt-5.4"
        "gpt-5.1-codex-mini"

        # Google Gemini
        "gemini-3.1-pro-preview"
        "gemini-3.1-flash-lite"
      ];
    };

    providers = {
      synthetic = {
        baseUrl = "https://api.synthetic.new/openai/v1";
        apiKey = "SYNTHETIC_API_KEY";
        api = "openai-completions";
        models = [
          {
            id = "hf:moonshotai/Kimi-K2.5";
            name = "Kimi K2.5 (Synthetic)";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 262144;
            maxTokens = 262144;
            cost = {
              input = 0;
              output = 0;
              cacheRead = 0;
              cacheWrite = 0;
            };
          }
          {
            id = "hf:zai-org/GLM-4.7";
            name = "GLM 4.7 (Synthetic)";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 202752;
            maxTokens = 8192;
            cost = {
              input = 0;
              output = 0;
              cacheRead = 0;
              cacheWrite = 0;
            };
          }
          {
            id = "hf:MiniMaxAI/MiniMax-M2.5";
            name = "MiniMax M2.5 (Synthetic)";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 1000000;
            maxTokens = 32768;
            cost = {
              input = 0;
              output = 0;
              cacheRead = 0;
              cacheWrite = 0;
            };
          }
        ];
      };
      fireworks = {
        baseUrl = "https://api.fireworks.ai/inference/v1";
        apiKey = "FIREWORKS_API_KEY";
        api = "openai-completions";
        models = [
          {
            id = "accounts/fireworks/models/kimi-k2p5";
            name = "Kimi K2.5 (Fireworks)";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 262144;
            maxTokens = 262144;
            cost = {
              input = 0.6;
              output = 3.0;
              cacheRead = 0;
              cacheWrite = 0;
            };
          }
          {
            id = "accounts/fireworks/models/glm-5";
            name = "GLM 5 (Fireworks)";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 202752;
            maxTokens = 8192;
            cost = {
              input = 1.0;
              output = 3.2;
              cacheRead = 0;
              cacheWrite = 0;
            };
          }
        ];
      };
      openai = {
        baseUrl = "https://api.openai.com/v1";
        apiKey = "OPENAI_API_KEY";
        api = "openai-completions";
        models = [
          {
            id = "gpt-5.4";
            name = "GPT-5.4";
            reasoning = true;
            input = [ "text" "image" ];
            contextWindow = 1050000;
            maxTokens = 128000;
            cost = {
              input = 2.5;
              output = 15.0;
              cacheRead = 0.25;
              cacheWrite = 0.0;
            };
          }
          {
            id = "gpt-5.1-codex-mini";
            name = "GPT-5.1 Codex Mini";
            reasoning = true;
            input = [ "text" "image" ];
            contextWindow = 400000;
            maxTokens = 100000;
            cost = {
              input = 0.25;
              output = 2.0;
              cacheRead = 0.025;
              cacheWrite = 0.0;
            };
          }
        ];
      };
    };

    keybindings = {
      # Cursor Movement (Emacs)
      cursorUp = [
        "up"
        "ctrl+p"
      ];
      cursorDown = [
        "down"
        "ctrl+n"
      ];
      cursorLeft = [
        "left"
        "ctrl+b"
      ];
      cursorRight = [
        "right"
        "ctrl+f"
      ];
      cursorWordLeft = [
        "alt+left"
        "ctrl+left"
        "alt+b"
      ];
      cursorWordRight = [
        "alt+right"
        "ctrl+right"
        "alt+f"
      ];
      cursorLineStart = [
        "home"
        "ctrl+a"
      ];
      cursorLineEnd = [
        "end"
        "ctrl+e"
      ];

      # Deletion (Emacs)
      deleteCharBackward = [
        "backspace"
        "ctrl+h"
      ];
      deleteCharForward = [
        "delete"
        "ctrl+d"
      ];
      deleteWordBackward = [
        "ctrl+w"
        "alt+backspace"
      ];
      deleteWordForward = [
        "alt+d"
        "alt+delete"
      ];
      deleteToLineStart = [ "ctrl+u" ];
      deleteToLineEnd = [ "ctrl+k" ];

      # Text Input
      newLine = [
        "shift+enter"
        "ctrl+j"
      ];
      submit = [ "enter" ];
      tab = [ "tab" ];

      # Kill Ring (Emacs)
      yank = [ "ctrl+y" ];
      yankPop = [ "alt+y" ];
      undo = [
        "ctrl+_"
        "ctrl+/"
      ];

      # Application
      interrupt = [ "escape" ];
      clear = [ "ctrl+c" ];
      exit = [ "ctrl+d" ];
      externalEditor = [ "ctrl+g" ];

      # Session
      selectModel = [ "ctrl+l" ];
      cycleModelForward = [ "ctrl+period" ];
      cycleModelBackward = [ "ctrl+comma" ];
      cycleThinkingLevel = [ "shift+tab" ];

      # Display
      expandTools = [ "ctrl+o" ];
      toggleThinking = [ "ctrl+t" ];

      # Message Queue
      followUp = [ "alt+enter" ];
      dequeue = [ "alt+up" ];
    };
  };

  home.file = {
    ".pi/agent/skills/home-manager".source = skillsDir;
    ".pi/agent/extensions/safety-gate.ts".text = safetyGateTs;
    ".pi/agent/extensions/safety-gate.json".text = safetyGateJson;
  };
}
