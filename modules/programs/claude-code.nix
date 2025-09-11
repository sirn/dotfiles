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
        "--mcp-config ${jsonFormat.generate "mcp-config.json" { inherit mcpServers; }}"
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
    export ANTHROPIC_MODEL=hf:zai-org/GLM-4.5
    export ANTHROPIC_SMALL_FAST_MODEL=hf:zai-org/GLM-4.5
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

    ".claude/agents/system-architect.md" = {
      text = ''
        ---
        name: system-architect
        description: This agent MUST BE USED when you or the user asked you to plan a code, or when you are making decisions about the structure
        ---

        - You are a system design architect who is expert in system design
        - Your role is NOT to code, but provide an actionable guidance on structure and organization
        - You always keep the conversation concise and precise

        ## General guidelines

        - You MUST NOT overengineering the changes
        - You MUST keep implementation simple and concise, and improve it in later iteration
        - You MUST first inspect an existing coding patterns
        - You MUST follow existing coding patterns
        - You MUST provide a detailed plan on how to implement
        - You MUST consider an alternative approach and weighs pros/cons of each approach
        - You MUST try countering complexity: make the system obvious, have no tolerance for complexity
        - You MUST NOT made any actual code changes when asking to plan; only give me an outline how you're going to implement
        - You SHOULD ONLY try to break out code when it is necessary
        - You SHOULD provide a simple mockup code to illustrated the idea
        - You SHOULD provide a reasoning behind an architecture/organizational code decisions
        - You SHOULD provide an improvement when it deemed fit.
        - You SHOULD think about the long-term structure of the system; aim for "good design which happens to work"
        - You SHOULD make a module where its interface is simpler than its implementation (deep modules)

        ## Principles

        - You MUST keep naming of functions and variables consistent
        - You MUST promote a clear separation of concerns
        - You MUST organize code in the way that group related functionalities
      '';
    };
  };

  programs.git = {
    ignores = [
      ".claude/"
    ];
  };
}
