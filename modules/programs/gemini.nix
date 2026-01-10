{ config, lib, pkgs, ... }:

let
  cfg = config.programs.gemini-cli;
in
{
  programs.gemini-cli = {
    enable = true;

    package = (pkgs.writeScriptBin "gemini" ''
      #!${pkgs.runtimeShell}
      exec "${lib.getExe pkgs.local.envWrapper}" \
        -i ~/.config/llm-agent/env \
        -- "${lib.getExe pkgs.unstable.gemini-cli}" "$@"
    '');

    # In 25.11, defaultModel only accepts string and default to gemini-2.5-pro
    # We want to use the best Auto model; so this needs to be set to an empty string.
    #
    # TODO: switch to null, >25.11
    defaultModel = "";

    settings = {
      general = {
        enablePromptCompletion = true;
        previewFeatures = true;
      };
      security = {
        auth = {
          selectedType = "gemini-api-key";
        };
        disableYoloMode = true;
      };
      tools = {
        autoAccept = true;
        allowed = [
          "ReadFileTool(*)"
          "GlobTool(*)"
          "GrepTool(*)"
          "ShellTool(cat)"
          "ShellTool(ls)"
          "ShellTool(find)"
          "ShellTool(grep)"
          "ShellTool(rg)"
          "ShellTool(fd)"
          "ShellTool(curl)"
          "ShellTool(wget)"
          "ShellTool(git status)"
          "ShellTool(git diff)"
          "ShellTool(git log)"
          "ShellTool(git branch)"
          "ShellTool(jj status)"
          "ShellTool(jj diff)"
          "ShellTool(jj log)"
        ];
        exclude = [
          "ShellTool(sudo)"
          "ShellTool(kill)"
          "ShellTool(systemctl)"
          "ShellTool(chmod)"
          "ShellTool(chown)"
          "ShellTool(rm)"
          "ShellTool(sops)"
        ];
      };
    };
  };

  programs.git = lib.mkIf cfg.enable {
    ignores = [
      ".gemini/"
    ];
  };
}
