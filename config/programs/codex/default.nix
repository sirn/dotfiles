{ config, lib, pkgs, ... }:

let
  cfg = config.programs.codex;

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

  toCodexConfig = mode:
    let
      policy = effectivePolicy mode;
      inherit (policy) tools;
    in
    {
      approval_policy = "untrusted";
      sandbox_mode = if tools.edit then "workspace-write" else "workspace-read-only";
      allow_login_shell = false;
      network = false;
    };

  toCodexRules = mode:
    let
      policy = effectivePolicy mode;
      inherit (policy) commands;
      mkPrefixRule = decision: cmd: ''
        prefix_rule(
            pattern = ["${lib.concatStringsSep ''", "'' (lib.splitString " " cmd)}"],
            decision = "${decision}",
        )'';
      forbiddenRules = map (mkPrefixRule "forbidden") (commands.deny.shell or [ ]);
      promptRules = map (mkPrefixRule "prompt") (commands.ask.shell or [ ]);
    in
    lib.concatStringsSep "\n\n" (forbiddenRules ++ promptRules);

  isStdioServer = server: server ? command || server ? package;

  toCodexMcpServers = servers:
    lib.mapAttrs
      (name: server:
        if isStdioServer server then {
          command = server.command or (lib.getExe server.package);
        } else {
          command = lib.getExe pkgs.local.mcpServers.mcp-remote;
          args = [ server.url ];
        })
      servers;

  tomlFormat = pkgs.formats.toml { };

  codexConfig = toCodexConfig "build";
  rulesContent = toCodexRules "build";

  baseSettings = {
    inherit (codexConfig) approval_policy sandbox_mode allow_login_shell network;
    mcp_servers = toCodexMcpServers config.programs.mcp.servers;
  } // cfg.settingsOverride;

  nixConfig = tomlFormat.generate "codex-config-nix" baseSettings;
in
{
  programs.codex = {
    package = pkgs.unstable.codex;
    custom-instructions = instructionText;
  };

  programs.git = {
    ignores = [
      ".codex/"
    ];
  };

  home.file.".codex/skills/home-manager".source = skillsDir;
  home.file.".codex/rules/default.rules".text = rulesContent;

  # Codex rewrites config.toml every time it's run in a new directory,
  # so we need to merge the local config with Nix-generated config on activation.
  # https://github.com/openai/codex/issues/5160
  home.activation.mergeCodexConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    mergeCodexConfig() {
      localConfig="$HOME/.codex/config.toml"
      nixConfig="${nixConfig}"

      if [[ -f "$localConfig" ]]; then
        echo "Merging local ~/.codex/config.toml with Nix-generated config..."
        ${lib.getExe' pkgs.tomlplusplus "toml_merger"} "$localConfig" "$nixConfig" > "$localConfig.tmp"
        mv "$localConfig.tmp" "$localConfig"
      else
        echo "Creating ~/.codex/config.toml from Nix-generated config..."
        cp "$nixConfig" "$localConfig"
      fi
    }

    mergeCodexConfig
  '';
}
