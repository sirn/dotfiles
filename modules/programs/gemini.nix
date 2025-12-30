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
      };
    };
  };

  programs.git = lib.mkIf cfg.enable {
    ignores = [
      ".gemini/"
    ];
  };
}
