{ config, lib, pkgs, ... }:

let
  cfg = config.programs.codex;

  instructionText = builtins.readFile ../../var/agents/instruction.md;

  skillsDir = ../../var/agents/skills;

  permissionsPolicy = builtins.fromTOML (builtins.readFile ../../var/agents/permissions.toml);

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
  options.programs.codex.settingsOverride = lib.mkOption {
    type = lib.types.attrs;
    default = { };
    description = ''
      Settings to write to the Codex configuration file.
      These will be merged with the local config on activation.
    '';
  };

  config = lib.mkIf cfg.enable {
    programs.codex = {
      package = pkgs.unstable.codex;
      custom-instructions = instructionText + ''

## Security and Permission Policy
This agent operates under the following constraints enforced via Codex rules and sandbox settings:

### Forbidden Commands (Always Denied)
- sudo, kill, systemctl, chown, sops
- git push, jj git push

### Prompt Required (Ask Before Execute)
- rm, chmod
- git commit
- jj describe, jj new, jj commit, jj squash, jj split, jj abandon, jj undo

### Path Restrictions (Enforced via Policy)
**Allowed**:
- Any env file ending in .env.example or .env.sample

**Denied** (read/write/edit):
- .env, .env.*, *.env
- Secret files: *.pem, *.key, *.p12, *.pfx, *.jks, id_rsa, id_ed25519
- Credential files: *credential*.json, *credentials*.json, *secret*, *token*, *apikey*, *api_key*
- Cloud creds: .aws/credentials, .npmrc, .pypirc, .netrc, .docker/config.json
- SOPS/Age: sops*.yml, sops*.yaml, *.agekey

### Note on Codex Limitations
Codex does not provide file-path-level deny policies equivalent to OpenCode or Claude Code. 
Secret file protections are enforced through a combination of:
- Workspace sandbox boundaries (sandbox_mode = workspace-write)
- Approval policy (untrusted - requires confirmation for unknown commands)
- Login shell disabled (allow_login_shell = false)
- The rules above (for command-level prompt/forbid)
- Repository hygiene and process (manual review for secret file access)

For full protection, ensure sensitive files are outside the workspace or excluded from Codex access.
      '';
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
  };
}
