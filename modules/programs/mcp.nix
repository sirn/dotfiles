{ config, lib, pkgs, ... }:

let
  braveMcpWrapper = pkgs.writeScriptBin "brave-mcp-wrapper" ''
    #!${pkgs.runtimeShell}
    exec "${lib.getExe pkgs.local.envWrapper}" \
      -i "''${XDG_CONFIG_HOME:-''${HOME}/.config}/sops-nix/secrets/agents/env" \
      -- ${lib.getExe pkgs.local.mcpServers.brave-search} --transport stdio
  '';

  # MCP server allowed tools (null = all tools via wildcard)
  allowedTools = {
    "context7" = null;      # All tools (read-only server)
    "brave-search" = null;  # All tools (read-only server)
  };

  # Generate Claude Code permissions from allowedTools
  claudeCodePermissions = lib.flatten (lib.mapAttrsToList (server: tools:
    if tools == null
    then [ "mcp__${server}__*" ]
    else map (tool: "mcp__${server}__${tool}") tools
  ) allowedTools);

  # Generate Gemini CLI server permissions from allowedTools
  geminiMcpPermissions = lib.mapAttrs (server: tools:
    if tools == null
    then { trust = true; }
    else { } # Do not restrict tools (allow write), but do not trust (ask for everything)
  ) allowedTools;

  # Generate OpenCode permissions from allowedTools
  opencodePermissions = lib.listToAttrs (lib.flatten (lib.mapAttrsToList (server: tools:
    if tools == null
    then [{ name = "${server}_*"; value = true; }]
    else
      # Allow read-only tools, ask for everything else (wildcard)
      [{ name = "${server}_*"; value = false; }] ++
      (map (tool: { name = "${server}_${tool}"; value = true; }) tools)
  ) allowedTools));
in
{
  programs.mcp = {
    enable = true;
    servers = {
      context7 = {
        command = lib.getExe pkgs.local.mcpServers.context7;
      };
      brave-search = {
        command = lib.getExe braveMcpWrapper;
      };
    };
  };

  # Claude Code MCP permissions
  programs.claude-code = lib.mkIf config.programs.claude-code.enable {
    settings.permissions.allow = claudeCodePermissions;
  };

  # Gemini CLI MCP permissions
  programs.gemini-cli = lib.mkIf config.programs.gemini-cli.enable {
    settings.mcpServers = geminiMcpPermissions;
  };

  # OpenCode MCP permissions
  programs.opencode = lib.mkIf config.programs.opencode.enable {
    settings.tools = opencodePermissions;
  };
}
