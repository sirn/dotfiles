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
