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

  agentPermissionsPath = ../../../var/agents/permissions.pi.toml;
  agentPermissions =
    if builtins.pathExists agentPermissionsPath then lib.importTOML agentPermissionsPath else { };

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
  };

  # Generate JSON config for plan-mode extension (plan-specific entries only)
  planModeJson = builtins.toJSON {
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
    wrappers = (permissionsToml.mode.plan.commands.wrappers or [ ]);
  };

  # Write JSON files to store paths (safer than echo in shell)
  safetyGateJsonFile = pkgs.writeTextFile {
    name = "safety-gate.json";
    text = safetyGateJson;
  };

  planModeJsonFile = pkgs.writeTextFile {
    name = "plan-mode.json";
    text = planModeJson;
  };

  # Combine bundled extensions with generated JSON configs into a single directory
  bundledExtensions = pkgs.runCommand "pi-bundled-extensions" { } ''
    mkdir -p $out
    cp -r ${./extensions}/. $out/
    cp ${safetyGateJsonFile} $out/safety-gate.json
    cp ${planModeJsonFile} $out/plan-mode.json
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
      fireworks-ai = {
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
            id = "accounts/fireworks/routers/kimi-k2p5-turbo";
            name = "Kimi K2.5 Turbo (Developer Pass)";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 256000;
            maxTokens = 256000;
            cost = {
              input = 0;
              output = 0;
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
      anthropic = {
        baseUrl = "https://api.anthropic.com";
        apiKey = "ANTHROPIC_API_KEY";
        api = "anthropic-messages";
        models = [
          {
            id = "claude-opus-4-6";
            name = "Claude Opus 4.6";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 200000;
            maxTokens = 128000;
            cost = {
              input = 5.0;
              output = 25.0;
              cacheRead = 0.5;
              cacheWrite = 6.25;
            };
          }
          {
            id = "claude-sonnet-4-6";
            name = "Claude Sonnet 4.6";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 200000;
            maxTokens = 64000;
            cost = {
              input = 3.0;
              output = 15.0;
              cacheRead = 0.3;
              cacheWrite = 3.75;
            };
          }
        ];
      };
      openai = {
        baseUrl = "https://api.openai.com/v1";
        apiKey = "OPENAI_API_KEY";
        api = "openai-responses";
        models = [
          {
            id = "gpt-5.4";
            name = "GPT-5.4";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
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
            id = "gpt-5.4-mini";
            name = "GPT-5.4 Mini";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 400000;
            maxTokens = 128000;
            cost = {
              input = 0.75;
              output = 4.5;
              cacheRead = 0.075;
              cacheWrite = 0.0;
            };
          }
          {
            id = "gpt-5.4-nano";
            name = "GPT-5.4 Nano";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 400000;
            maxTokens = 128000;
            cost = {
              input = 0.2;
              output = 1.25;
              cacheRead = 0.02;
              cacheWrite = 0.0;
            };
          }
        ];
      };
      google = {
        baseUrl = "https://generativelanguage.googleapis.com/v1beta";
        apiKey = "GEMINI_API_KEY";
        api = "google-generative-ai";
        models = [
          {
            id = "gemini-3.1-pro-preview";
            name = "Gemini 3.1 Pro Preview";
            reasoning = true;
            input = [
              "text"
              "image"
            ];
            contextWindow = 1048576;
            maxTokens = 65536;
            cost = {
              input = 2.0;
              output = 12.0;
              cacheRead = 0.2;
              cacheWrite = 0;
            };
          }
          {
            id = "gemini-3.1-flash-lite-preview";
            name = "Gemini 3.1 Flash Lite Preview";
            reasoning = false;
            input = [
              "text"
              "image"
            ];
            contextWindow = 1048576;
            maxTokens = 65536;
            cost = {
              input = 0.5;
              output = 3.0;
              cacheRead = 0.05;
              cacheWrite = 0;
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
    ".pi/agent/extensions/home-manager".source = bundledExtensions;
  };
}
