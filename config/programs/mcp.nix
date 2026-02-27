{ config, lib, pkgs, ... }:

let
  braveMcpWrapper = pkgs.writeScriptBin "brave-mcp-wrapper" ''
    #!${pkgs.runtimeShell}
    exec "${lib.getExe pkgs.local.envWrapper}" \
      -i "''${XDG_CONFIG_HOME:-''${HOME}/.config}/sops-nix/secrets/agents/env" \
      -- ${lib.getExe pkgs.local.mcpServers.brave-search} --transport stdio
  '';
in
{
  programs.mcp = {
    enable = true;
    servers = {
      context7 = {
        command = lib.getExe pkgs.local.mcpServers.context7;
        allowedTools = null;      # All tools (read-only server)
      };
      brave-search = {
        command = lib.getExe braveMcpWrapper;
        allowedTools = null;    # All tools (read-only server)
      };
    };
  };
}
