{ lib, pkgs, ... }:

let
  package = pkgs.unstable.opencode;

  mcpServers = {
    context7 = {
      type = "local";
      enabled = true;
      command = [
        (lib.getExe pkgs.local.mcpServers.context7)
      ];
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
      exec "${lib.getExe pkgs.local.envWrapper}" \
        -i ~/.config/llm-agent/env \
        -a GOOGLE_GENERATIVE_AI_API_KEY=GEMINI_API_KEY \
        -- "${package}/bin/opencode" "$@"
    '')
  ];

  xdg.configFile = {
    "opencode/opencode.json" = {
      text = builtins.toJSON {
        "$schema" = "https://opencode.ai/config.json";
        autoupdate = false;
        mcp = mcpServers;
        permission = {
          bash = "ask";
        };
      };
    };
  };
}
