{ lib, pkgs, ... }:

let
  package = pkgs.unstable.gemini-cli;

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
        type = "stdio";
        command = lib.getExe context7McpWrapper;
      };
  };
in
{
  home.packages = with pkgs; [
    (pkgs.writeScriptBin "gemini" ''
      #!${pkgs.runtimeShell}
      if [ -f $HOME/.config/llm-agent/env ]; then
        set -a
        source $HOME/.config/llm-agent/env
        set +a
      fi
      exec "${package}/bin/gemini" "$@"
    '')
  ];

  home.file = {
    ".gemini/settings.json" = {
      text = builtins.toJSON {
        selectedAuthType = "gemini-api-key";
        disableAutoUpdate = true;
        theme = "ANSI";
        mcpServers = mcpServers;
      };
    };
  };

  programs.git = {
    ignores = [
      ".gemini/"
    ];
  };
}
