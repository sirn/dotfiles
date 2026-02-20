{ config, lib, pkgs, ... }:

let
  cfg = config.programs.codex;

  instructionText = builtins.readFile ../../var/agents/instruction.md;

  skillsDir = ../../var/agents/skills;

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

  baseSettings = {
    approval_policy = "on-request";
    sandbox_mode = "workspace-write";
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
      custom-instructions = instructionText;
    };

    programs.git = {
      ignores = [
        ".codex/"
      ];
    };

    home.file.".codex/skills/home-manager".source = skillsDir;

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
