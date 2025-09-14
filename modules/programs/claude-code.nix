{ lib, pkgs, ... }:

let
  package = pkgs.unstable.claude-code;

  jsonFormat = pkgs.formats.json { };

  mcpServers = {
    context7 = {
      type = "stdio";
      command = lib.getExe pkgs.local.mcpServers.context7;
    };

    brave-search =
      let
        braveMcpWrapper = pkgs.writeScriptBin "brave-mcp-wrapper" ''
          #!${pkgs.runtimeShell}
          exec "${lib.getExe pkgs.local.envWrapper}" \
            -i ~/.config/llm-agent/env \
            -- ${lib.getExe pkgs.local.mcpServers.brave-search} --transport stdio
        '';
      in
      {
        type = "stdio";
        command = lib.getExe braveMcpWrapper;
      };

    fetch = {
      type = "stdio";
      command = lib.getExe pkgs.local.mcpServers.fetch;
    };
  };

  wrappedClaude = pkgs.stdenv.mkDerivation {
    pname = "wrapped-${package.name}";
    src = ./.;
    version = package.version;

    nativeBuildInputs = [
      pkgs.makeWrapper
    ];

    installPhase = ''
      mkdir -p $out/bin
      makeWrapper ${package}/bin/claude $out/bin/claude ${lib.escapeShellArgs [
        "--add-flags"
        "--mcp-config=${jsonFormat.generate "mcp-config.json" { inherit mcpServers; }}"
      ]}
    '';
  };

  wrappedClaudeSynthetic = pkgs.writeScriptBin "claude-synthetic" ''
    #!${pkgs.runtimeShell}
    # Run Claude Code with Synthetic
    if [ -f "$XDG_CONFIG_HOME/llm-agent/env" ]; then
        . "$XDG_CONFIG_HOME/llm-agent/env"
    fi

    export ANTHROPIC_BASE_URL=https://api.synthetic.new/anthropic
    export ANTHROPIC_AUTH_TOKEN=$SYNTHETIC_API_KEY
    export ANTHROPIC_DEFAULT_OPUS_MODEL=hf:zai-org/GLM-4.5
    export ANTHROPIC_DEFAULT_SONNET_MODEL=hf:zai-org/GLM-4.5
    export ANTHROPIC_DEFAULT_HAIKU_MODEL=hf:zai-org/GLM-4.5
    export CLAUDE_CODE_SUBAGENT_MODEL=hf:zai-org/GLM-4.5
    export CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC=1
    export CLAUDE_CONFIG_DIR=$XDG_CONFIG_HOME/claude-synthetic

    exec ${wrappedClaude}/bin/claude "$@"
  '';
in
{
  home.packages = with pkgs; [
    wrappedClaude
    wrappedClaudeSynthetic
  ];

  home.file = {
    ".claude/settings.json" = {
      text = builtins.toJSON {
        "$schema" = "https://json.schemastore.org/claude-code-settings.json";
        model = "opusplan";
        includeCoAuthoredBy = false;
        cleanupPeriodDays = 7;
      };
    };
  };

  programs.git = {
    ignores = [
      ".claude/"
    ];
  };
}
