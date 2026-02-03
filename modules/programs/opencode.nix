{ config, lib, pkgs, ... }:

let
  instructionText = builtins.readFile ../../var/agents/instruction.md;

  skillsDir = ../../var/agents/skills;

  agentsDir = ../../var/agents/agents;

  agentFiles = builtins.filter
    (name: lib.hasSuffix ".toml" name)
    (builtins.attrNames (builtins.readDir agentsDir));

  loadAgent = tomlFile:
    let
      name = lib.removeSuffix ".toml" tomlFile;
      agentConfig = builtins.fromTOML (builtins.readFile (agentsDir + "/${tomlFile}"));
      prompt = builtins.readFile (agentsDir + "/${name}.md");
    in
    {
      inherit name;
      value = agentConfig // { inherit prompt; };
    };

  agents = builtins.listToAttrs (map loadAgent agentFiles);

  mkOpencodeAgent = name: agent: ''
    ---
    name: ${name}
    description: ${agent.description}
    model: ${agent.opencode.model}
    mode: subagent
    ---
    ${agent.prompt}
  '';

  isStdioServer = server: server ? command || server ? package;

  toOpencodeMcpServers = servers:
    lib.mapAttrs
      (name: server:
        if isStdioServer server then {
          command = [ (server.command or (lib.getExe server.package)) ];
          type = "local";
          enabled = true;
        } else {
          url = server.url;
          type = "remote";
          enabled = true;
        })
      servers;
in
{
  programs.opencode = {
    enable = true;

    package = (pkgs.writeScriptBin "opencode" ''
      #!${pkgs.runtimeShell}
      exec "${lib.getExe pkgs.local.envWrapper}" \
        -i "''${XDG_CONFIG_HOME:-$HOME/.config}/agents/env" \
        -a GOOGLE_GENERATIVE_AI_API_KEY=GEMINI_API_KEY \
        -- "${lib.getExe pkgs.unstable.opencode}" "$@"
    '');

    agents = lib.mapAttrs mkOpencodeAgent agents;
    rules = instructionText;

    settings = {
      mcp = toOpencodeMcpServers config.programs.mcp.servers;
      mode = {
        plan.model = "synthetic/hf:zai-org/GLM-4.7";
        build.model = "synthetic/hf:MiniMaxAI/MiniMax-M2.1";
      };
      permission = {
        read = {
          "*" = "allow";
          "*.env" = "deny";
          ".env" = "deny";
          "*.env.*" = "deny";
          "*.env.example" = "allow";
          "*.env.sample" = "allow";
        };
        glob = "allow";
        grep = "allow";
        list = "allow";
        bash = {
          "*" = "ask";
          "cat *" = "allow";
          "ls *" = "allow";
          "ls" = "allow";
          "find *" = "allow";
          "grep *" = "allow";
          "rg *" = "allow";
          "fd *" = "allow";
          "git status *" = "allow";
          "git status" = "allow";
          "git diff *" = "allow";
          "git diff" = "allow";
          "git log *" = "allow";
          "git log" = "allow";
          "git branch *" = "allow";
          "git branch" = "allow";
          "jj status *" = "allow";
          "jj status" = "allow";
          "jj diff *" = "allow";
          "jj diff" = "allow";
          "jj log *" = "allow";
          "jj log" = "allow";
          "jj show *" = "allow";
          "jj show" = "allow";
          "go test *" = "allow";
          "go test" = "allow";
          "go build *" = "allow";
          "go build" = "allow";
          "curl *" = "allow";
          "curl" = "allow";
          "wget *" = "allow";
          "tree *" = "allow";
          "tree" = "allow";
          "lstr *" = "allow";
          "sudo *" = "deny";
          "kill *" = "deny";
          "systemctl *" = "deny";
          "chmod *" = "ask";
          "chown *" = "deny";
          "git push *" = "deny";
          "jj git push *" = "deny";
        };
        edit = {
          "*" = "allow";
          "*.env" = "deny";
          ".env" = "deny";
          "*.env.*" = "deny";
          "*.env.example" = "allow";
          "*.env.sample" = "allow";
        };
        webfetch = "allow";
        websearch = "allow";
      };
    };
  };

  xdg.configFile."opencode/skill/home-manager".source = skillsDir;

  xdg.configFile."opencode/plugins/notify-turn-complete.ts".source =
    lib.mkIf config.programs.opencode.enable
      (pkgs.writeText "notify-turn-complete.ts" ''
        import type { Plugin } from "@opencode-ai/plugin"

        export const NotifyTurnComplete: Plugin = async ({ project, client, $, directory, worktree }) => {
          return {
            event: async ({ event }) => {
              if (event.type === "session.idle") {
                await $`${lib.getExe pkgs.toastify} send "OpenCode" "OpenCode finished their turn"`
              }
            },
          }
        }
      '');
}
