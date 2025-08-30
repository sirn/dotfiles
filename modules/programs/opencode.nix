{ lib, pkgs, ... }:

let
  package = pkgs.unstable.opencode;

  mcpServers = {
    context7 =
      let
        context7McpWrapper = pkgs.writeScriptBin "context7-mcp-wrapper" ''
          #!${pkgs.runtimeShell}
          export PATH=${pkgs.nodejs}/bin:$PATH
          exec ${pkgs.nodejs}/bin/npx -y @upstash/context7-mcp@latest
        '';
      in
      {
        type = "local";
        enabled = true;
        command = [
          (lib.getExe context7McpWrapper)
        ];
      };

    brave-search =
      let
        braveMcpWrapper = pkgs.writeScriptBin "brave-mcp-wrapper" ''
          #!${pkgs.runtimeShell}
          export PATH=${pkgs.nodejs}/bin:$PATH
          if [ -f $HOME/.config/llm-agent/env ]; then
            set -a
            source $HOME/.config/llm-agent/env
            set +a
          fi
          exec ${pkgs.nodejs}/bin/npx -y @brave/brave-search-mcp-server --transport stdio
        '';
      in
      {
        type = "local";
        enabled = true;
        command = [
          (lib.getExe braveMcpWrapper)
        ];
      };
  };
in
{
  home.packages = with pkgs; [
    (pkgs.writeScriptBin "opencode" ''
      #!${pkgs.runtimeShell}
      if [ -f $HOME/.config/llm-agent/env ]; then
        set -a
        source $HOME/.config/llm-agent/env
        set +a
      fi
      export GOOGLE_GENERATIVE_AI_API_KEY=$GEMINI_API_KEY
      exec "${package}/bin/opencode" "$@"
    '')
  ];

  xdg.configFile = {
    "opencode/opencode.json" = {
      text = builtins.toJSON {
        "$schema" = "https://opencode.ai/config.json";
        theme = "system";
        autoupdate = false;
        mcp = mcpServers;
        permission = {
          bash = "ask";
        };
      };
    };
  };
}
