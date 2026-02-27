{ config, lib, pkgs, ... }:

let
  cfg = config.programs.gemini-cli;

  instructionText = builtins.readFile ../../../var/agents/instruction.md;

  skillsDir = ../../../var/agents/skills;

  permissionsPolicy = builtins.fromTOML (builtins.readFile ../../../var/agents/permissions.toml);

  effectivePolicy = mode:
    let
      default = permissionsPolicy.default or { };
      modePolicy = permissionsPolicy.mode.${mode} or { };
      mergeLists = lists: lib.unique (lib.concatLists (lib.filter (l: l != null) lists));
      mergeCmds = section:
        let
          defaultShell = (default.commands.${section} or { }).shell or [ ];
          modeShell = ((modePolicy.commands or { }).${section} or { }).shell or [ ];
        in
        { shell = mergeLists [ defaultShell modeShell ]; };
    in
    {
      tools = default.tools // (modePolicy.tools or { });
      commands = {
        allow = mergeCmds "allow";
        ask = mergeCmds "ask";
        deny = mergeCmds "deny";
      };
      paths = default.paths;
    };

  toGeminiPermissions = mode:
    let
      policy = effectivePolicy mode;
      inherit (policy) tools commands paths;

      baseAllowed = [
        "ReadFileTool(**)"
        "GlobTool(*)"
        "GrepTool(*)"
        "ShellTool(ls)"
        "ShellTool(fd)"
        "ShellTool(find)"
        "ShellTool(rg)"
        "ShellTool(grep)"
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

      buildExtras = lib.optionals tools.edit [
        "Edit"
        "WriteFile"
        "ShellTool(tree)"
        "ShellTool(lstr)"
      ];

      allowed = baseAllowed ++ buildExtras;

      shellExcludes = map (cmd: "ShellTool(${cmd})") (commands.deny.shell ++ commands.ask.shell);

      pathExcludes =
        map (p: "ReadFileTool(${p})") (paths.deny.read or [ ])
        ++ lib.optionals tools.edit (map (p: "Edit(${p})") (paths.deny.edit or [ ]))
        ++ lib.optionals tools.write (map (p: "WriteFile(${p})") (paths.deny.write or [ ]));

      exclude = shellExcludes ++ pathExcludes;
    in
    { inherit allowed exclude; };

  isStdioServer = server: server ? command || server ? package;

  toGeminiMcpServers = servers:
    lib.mapAttrs
      (name: server:
        let
          tools = server.allowedTools or null;
          baseConfig = 
            if isStdioServer server then {
              command = server.command or (lib.getExe server.package);
            } else {
              command = lib.getExe pkgs.local.mcpServers.mcp-remote;
              args = [ server.url ];
            };
          # Add trust setting based on allowedTools
          trustConfig = 
            if tools == null
            then { trust = true; }
            else { };
        in
          baseConfig // trustConfig)
      servers;
in
{
  programs.gemini-cli = {
    enable = true;

    package = (pkgs.writeScriptBin "gemini" ''
      #!${pkgs.runtimeShell}
      exec "${lib.getExe pkgs.local.envWrapper}" \
        -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env" \
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
        inherit (toGeminiPermissions "build") allowed exclude;
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
