{ lib, pkgs, ... }:

let
  package = pkgs.unstable.gemini-cli;

  mcpServers = {
    context7 = {
      type = "stdio";
      command = lib.getExe pkgs.local.mcpServers.context7;
    };
  };
in
{
  home.packages = with pkgs; [
    (pkgs.writeScriptBin "gemini" ''
      #!${pkgs.runtimeShell}
      exec "${lib.getExe pkgs.local.envWrapper}" \
        -i ~/.config/llm-agent/env \
        -- "${lib.getExe package}" "$@"
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
