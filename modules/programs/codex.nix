{ config, lib, pkgs, ... }:

let
  cfg = config.programs.codex;

  instructionText = builtins.readFile ../../var/agents/instruction.md;

  skillsDir = ../../var/agents/skills;

  projectsFile = ../../var/projects.txt;
  projectPaths = lib.optionals (builtins.pathExists projectsFile)
    (lib.filter (s: s != "") (lib.splitString "\n" (builtins.readFile projectsFile)));

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
      approval_policy = "on-request";
      sandbox_mode = "workspace-write";

      projects = lib.mkMerge [
        (lib.genAttrs projectPaths (_: { trust_level = "trusted"; }))
        {
          "${config.home.homeDirectory}/.dotfiles" = {
            trust_level = "trusted";
          };
        }
        (lib.mkIf (pkgs.stdenv.isLinux && !config.targets.genericLinux.enable) {
          "/etc/nixos" = {
            trust_level = "trusted";
          };
        })
      ];

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
