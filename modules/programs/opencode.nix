{ lib, pkgs, ... }:

{
  programs.opencode = {
    enable = true;

    package = (pkgs.writeScriptBin "opencode" ''
      #!${pkgs.runtimeShell}
      exec "${lib.getExe pkgs.local.envWrapper}" \
        -i ~/.config/llm-agent/env \
        -a GOOGLE_GENERATIVE_AI_API_KEY=GEMINI_API_KEY \
        -- "${lib.getExe pkgs.unstable.opencode}" "$@"
    '');

    settings = {
      permission = {
        read = "allow";
        glob = "allow";
        grep = "allow";
        list = "allow";
        bash = {
          cat = "allow";
          ls = "allow";
          find = "allow";
          grep = "allow";
          rg = "allow";
          fd = "allow";
          "git status" = "allow";
          "git diff" = "allow";
          "git log" = "allow";
          "git branch" = "allow";
          "jj status" = "allow";
          "jj diff" = "allow";
          "jj log" = "allow";
          curl = "allow";
          wget = "allow";
          sudo = "deny";
          kill = "deny";
          systemctl = "deny";
          chmod = "deny";
          chown = "deny";
          "rm *" = "ask";
          "rm -rf *" = "ask";
          "sops *" = "ask";
          "mv *" = "ask";
          "git push" = "ask";
          "git commit" = "ask";
          "*" = "ask";
        };
        edit = {
          "./**/*" = "allow";
          tmp = "allow";
          "*" = "ask";
        };
        webfetch = "allow";
        websearch = "allow";
      };
    };
  };
}
