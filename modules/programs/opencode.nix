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
        bash = "ask";
        edit = "ask";
        webfetch = "allow";
      };
    };
  };
}
