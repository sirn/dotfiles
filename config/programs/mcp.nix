{ config, lib, pkgs, ... }:

{
  programs.mcp = {
    enable = true;
    servers = {
      context7 = {
        command = lib.getExe pkgs.local.mcpServers.context7;
        allowedTools = null;      # All tools (read-only server)
      };
    };
  };
}
