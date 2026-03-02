{ config, lib, pkgs, ... }:

let
  cfg = config.programs.pi-coding-agent;

  skillsDir = ../../../var/agents/skills;
  instructionText = builtins.readFile ../../../var/agents/instruction.md;
  permissionsToml = lib.importTOML ../../../var/agents/permissions.toml;

  wrappedPi = pkgs.writeScriptBin "pi" ''
    #!${pkgs.runtimeShell}
    exec "${lib.getExe pkgs.local.envWrapper}" \
      -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env" \
      -- "${lib.getExe pkgs.local.pi-coding-agent}" "$@"
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
      defaultThinkingLevel = "medium";
      hideThinkingBlock = true;
      enabledModels = [
        "accounts/fireworks/models/*"
        "hf:zai-org/*"
        "hf:moonshotai/*"
        "hf:MiniMaxAI/*"
        "claude-opus-4-6"
        "claude-sonnet-4-6"
        "gpt-5.3-codex"
        "gpt-5.3-codex-spark"
        "gemini-3.1-pro-preview"
        "gemini-3-flash-preview"
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
            input = [ "text" "image" ];
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
            input = [ "text" "image" ];
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
            input = [ "text" "image" ];
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
            input = [ "text" "image" ];
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
            input = [ "text" "image" ];
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
    };
  };

  home.file = {
    ".pi/agent/skills/home-manager".source = skillsDir;
    ".pi/agent/extensions/safety-gate.ts".text = safetyGateTs;
    ".pi/agent/extensions/safety-gate.json".text = safetyGateJson;
  };
}
