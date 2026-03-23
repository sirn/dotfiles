{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.codex;
  agentsCfg = config.agents;

  policy = agentsCfg.permissions.effective.build;

  toCodexConfig =
    let
      inherit (policy) tools;
    in
    {
      approval_policy = "untrusted";
      sandbox_mode = if tools.edit then "workspace-write" else "workspace-read-only";
      allow_login_shell = false;
      network = false;
    };

  toCodexRules =
    let
      inherit (policy) commands;
      mkRule =
        decision: entry:
        let
          m = entry.match;
          mode = entry.mode or "prefix";
        in
        if mode == "substring" then
          ''
            # NOTE: Cannot express substring match "${m}" as prefix_rule.
            # Blocked at sandbox level (network = false) or by agent instructions.''
        else
          ''
            prefix_rule(
                pattern = ["${lib.concatStringsSep ''", "'' (lib.splitString " " m)}"],
                decision = "${decision}",
            )'';
      forbiddenRules = map (mkRule "forbidden") (commands.deny or [ ]);
      promptRules = map (mkRule "prompt") (commands.ask or [ ]);
    in
    lib.concatStringsSep "\n\n" (forbiddenRules ++ promptRules);

  isStdioServer = server: server ? command || server ? package;

  toCodexMcpServers =
    servers:
    lib.mapAttrs (
      name: server:
      if isStdioServer server then
        { command = server.command or (lib.getExe server.package); }
      else
        { url = server.url; }
    ) servers;

  tomlFormat = pkgs.formats.toml { };

  codexConfig = toCodexConfig;
  rulesContent = toCodexRules;

  baseSettings = {
    inherit (codexConfig)
      approval_policy
      sandbox_mode
      allow_login_shell
      network
      ;
    model_reasoning_effort = "medium";
    mcp_servers = toCodexMcpServers config.programs.mcp.servers;
  }
  // cfg.settingsOverride;

  nixConfig = tomlFormat.generate "codex-config-nix" baseSettings;
in
{
  programs.codex = {
    enable = true;
    package = (
      pkgs.writeScriptBin "codex" ''
        #!${pkgs.runtimeShell}
        exec "${lib.getExe pkgs.local.envWrapper}" \
          -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env" \
          -- "${lib.getExe pkgs.local.codex-bin}" "$@"
      ''
    );
    custom-instructions = agentsCfg.instructionText;
  };

  programs.git = {
    ignores = [ ".codex/" ];
  };

  home.file.".codex/skills/home-manager".source = agentsCfg.skillsDir;
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
