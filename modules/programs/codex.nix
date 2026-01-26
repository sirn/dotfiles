{ config, lib, pkgs, ... }:

let
  cfg = config.programs.codex;

  skillsDir = ../../var/agents/skills;
  instructionText = builtins.readFile ../../var/agents/instruction.md;

  isStdioServer = server: server ? command || server ? package;

  toMcpRemoteTransport = transport:
    if transport == "http" then "http-only"
    else if transport == "sse" then "sse-only"
    else transport;

  toCodexMcpServers = servers:
    lib.mapAttrs
      (name: server:
        if isStdioServer server then {
          command = server.command or (lib.getExe server.package);
        } else {
          command = lib.getExe pkgs.local.mcpServers.mcp-remote;
          args = [ server.url "--transport" (toMcpRemoteTransport (server.transport or "sse")) ];
        })
      servers;
in
{
  programs.codex = {
    enable = true;

    package = pkgs.unstable.codex;

    custom-instructions = instructionText;

    settings = {
      projects = lib.mkMerge [
        {
          "${config.home.homeDirectory}/.dotfiles" = {
            trust_level = "untrusted";
          };
        }
        (lib.mkIf (pkgs.stdenv.isLinux && !config.targets.genericLinux.enable) {
          "/etc/nixos" = {
            trust_level = "untrusted";
          };
        })
      ];
      sandbox = "workspace-write";
      ask_for_approval = "on-failure";
      mcp_servers = toCodexMcpServers config.programs.mcp.servers;
    };
  };

  programs.git = lib.mkIf cfg.enable {
    ignores = [
      ".codex/"
    ];
  };

  home.file.".codex/skills/home-manager".source = lib.mkIf cfg.enable skillsDir;
}
