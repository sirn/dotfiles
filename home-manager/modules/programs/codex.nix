{ lib, pkgs, ... }:

let
  tomlFormat = pkgs.formats.toml { };
in
{
  options.programs.codex.settingsOverride = lib.mkOption {
    inherit (tomlFormat) type;
    default = { };
    description = ''
      Configuration settings passed to Codex via `-c` wrapper flags,
      leaving `~/.codex/config.toml` writable for runtime state.
    '';
  };
}
