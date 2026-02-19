{ config, lib, pkgs, ... }:

let
  cfg = config.programs.gemini-cli;

  instructionText = builtins.readFile ../../var/agents/instruction.md;

  skillsDir = ../../var/agents/skills;

  isStdioServer = server: server ? command || server ? package;

  toGeminiMcpServers = servers:
    lib.mapAttrs
      (name: server:
        if isStdioServer server then {
          command = server.command or (lib.getExe server.package);
        } else {
          command = lib.getExe pkgs.local.mcpServers.mcp-remote;
          args = [ server.url ];
        })
      servers;
in
{
  programs.gemini-cli = {
    enable = true;

    package = (pkgs.writeScriptBin "gemini" ''
      #!${pkgs.runtimeShell}
      exec "${lib.getExe pkgs.local.envWrapper}" \
        -i ~/.config/agents/env \
        -- "${lib.getExe pkgs.unstable.gemini-cli}" "$@"
    '');

    # In 25.11, defaultModel only accepts string and default to gemini-2.5-pro
    # We want to use the best Auto model; so this needs to be set to an empty string.
    #
    # TODO: switch to null, >25.11
    defaultModel = "";

    context.AGENTS = instructionText;

    settings = {
      mcpServers = toGeminiMcpServers config.programs.mcp.servers;
      context.fileName = [ "AGENTS.md" "GEMINI.md" ];
      general = {
        enablePromptCompletion = true;
        previewFeatures = true;
      };
      ui = {
        theme = "ANSI";
        autoThemeSwitching = true;
      };
      security = {
        auth = {
          # Valid Types:
          # oauth-personal: Login with Google
          # gemini-api-key: Gemini API key
          # vertex-ai: Vertex AI API key
          # compute-default-credentials: Google Cloud default credentials
          #
          # We set this to oauth-personal by default to use our Google subscription.
          selectedType = lib.mkDefault "oauth-personal";
        };
        disableYoloMode = true;
      };
      tools = {
        autoAccept = true;
        sandbox = pkgs.stdenv.isDarwin;
        allowed = [
          "ReadFileTool(**)"
          "GlobTool(*)"
          "GrepTool(*)"
          "Edit"
          "WriteFile"
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
          "ShellTool(jj show)"
          "ShellTool(go test)"
          "ShellTool(go build)"
        ];
        exclude = [
          "ShellTool(sudo)"
          "ShellTool(kill)"
          "ShellTool(systemctl)"
          "ShellTool(chown)"
          "ShellTool(rm)"
          "ShellTool(sops)"
          "ShellTool(git push)"
          "ShellTool(jj git push)"
          "ReadFileTool(**/.env)"
          "ReadFileTool(**/.env.*)"
          "ReadFileTool(**/*.env)"
          "Edit(**/.env)"
          "Edit(**/.env.*)"
          "Edit(**/*.env)"
          "WriteFile(**/.env)"
          "WriteFile(**/.env.*)"
          "WriteFile(**/*.env)"
        ];
      };
    };
  };

  programs.git = lib.mkIf cfg.enable {
    ignores = [
      ".gemini/"
    ];
  };

  home.file.".gemini/skills".source = lib.mkIf cfg.enable skillsDir;
}
