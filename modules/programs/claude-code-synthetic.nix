{ config, lib, pkgs, ... }:

let
  cfg = config.programs.claude-code;
in
lib.mkIf cfg.enable {
  home.packages = [
    (pkgs.writeScriptBin "synclaude" ''
      #!${pkgs.runtimeShell}
      set -euo pipefail
      ENV_FILE="''${XDG_CONFIG_HOME:-''${HOME}/.config}/llm-agent/env"
      if [ -f "$ENV_FILE" ]; then
          # shellcheck disable=SC1090
          . "$ENV_FILE"
      fi

      export SYNTHETIC_PLAN_MODEL="''${SYNTHETIC_PLAN_MODEL:-hf:zai-org/GLM-4.7}"
      export SYNTHETIC_BUILD_MODEL="''${SYNTHETIC_BUILD_MODEL:-hf:MiniMaxAI/MiniMax-M2.1}"
      export ANTHROPIC_BASE_URL=https://api.synthetic.new/anthropic
      export ANTHROPIC_AUTH_TOKEN="$SYNTHETIC_API_KEY"
      export ANTHROPIC_MODEL="opusplan"
      export ANTHROPIC_DEFAULT_OPUS_MODEL="''${ANTHROPIC_DEFAULT_OPUS_MODEL:-$SYNTHETIC_PLAN_MODEL}"
      export ANTHROPIC_DEFAULT_SONNET_MODEL="''${ANTHROPIC_DEFAULT_SONNET_MODEL:-$SYNTHETIC_BUILD_MODEL}"
      export ANTHROPIC_DEFAULT_HAIKU_MODEL="''${ANTHROPIC_DEFAULT_HAIKU_MODEL:-$SYNTHETIC_BUILD_MODEL}"
      export CLAUDE_CODE_SUBAGENT_MODEL="''${CLAUDE_CODE_SUBAGENT_MODEL:-$SYNTHETIC_BUILD_MODEL}"
      export CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC=1
      export CLAUDE_CONFIG_DIR="''${XDG_CONFIG_HOME}/claude-synthetic"

      exec ${lib.getExe cfg.finalPackage} "$@"
    '')

    (pkgs.writeScriptBin "synthetic-quota" ''
      #!${pkgs.runtimeShell}
      set -euo pipefail
      ENV_FILE="''${XDG_CONFIG_HOME:-''${HOME}/.config}/llm-agent/env"
      if [ -f "$ENV_FILE" ]; then
          # shellcheck disable=SC1090
          . "$ENV_FILE"
      fi

      if [ -z "''${SYNTHETIC_API_KEY:-}" ]; then
          echo >&2 "SYNTHETIC_API_KEY is not set"
          exit 1
      fi

      response=$(${lib.getExe pkgs.curl} -sf --max-time 10 \
          -H "Authorization: Bearer $SYNTHETIC_API_KEY" \
          https://api.synthetic.new/v2/quotas) || {
          echo >&2 "Failed to fetch quota information"
          exit 1
      }

      echo "$response" | ${lib.getExe pkgs.jq}
    '')
  ];
}