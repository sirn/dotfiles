{ config, lib, pkgs, ... }:

let
  skillsDir = ../../var/agents/skills;
  instructionText = builtins.readFile ../../var/agents/instruction.md;

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
}
