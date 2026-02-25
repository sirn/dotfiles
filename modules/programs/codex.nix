{ lib, ... }:

{
  options.programs.codex.settingsOverride = lib.mkOption {
    type = lib.types.attrs;
    default = { };
    description = ''
      Settings to write to the Codex configuration file.
      These will be merged with the local config on activation.
    '';
  };
}
