{ lib, pkgs, ... }:

{
  programs.opencode = {
    enable = true;

    package = (pkgs.writeScriptBin "opencode" ''
      #!${pkgs.runtimeShell}
      exec "${lib.getExe pkgs.local.envWrapper}" \
        -i "''${XDG_CONFIG_HOME:-$HOME/.config}/llm-agent/env" \
        -a GOOGLE_GENERATIVE_AI_API_KEY=GEMINI_API_KEY \
        -- "${lib.getExe pkgs.unstable.opencode}" "$@"
    '');

    settings = {
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
}
