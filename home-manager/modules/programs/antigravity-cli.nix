{ lib, pkgs, ... }:

let
  jsonFormat = pkgs.formats.json { };
in
{
  options.programs.antigravity-cli.settingsOverride = lib.mkOption {
    inherit (jsonFormat) type;
    default = { };
    description = ''
      Configuration settings merged onto the existing Antigravity CLI
      settings.json file on activation, preserving runtime changes such as
      trustedWorkspaces.
    '';
  };
}
