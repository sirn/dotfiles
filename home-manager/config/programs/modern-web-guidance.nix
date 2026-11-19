{ lib, pkgs, ... }:

let
  modernWebGuidanceInstructionText = lib.strings.trim ''
    - Use the `modern-web-guidance` skill (and its `modern-web-guidance` CLI) for HTML, CSS, and client-side JS tasks.
  '';

  modernWebGuidanceCommandContext = lib.strings.trim ''
    - `modern-web-guidance` — read-only CLI querying bundled web platform guides (`search`, `retrieve`, `list`) and scanning local files (`legacy-scan`); telemetry disabled by default
  '';
in
{
  agents.skillSets.modern-web-guidance = "${pkgs.local.modern-web-guidance}/skills";

  agents.permissions.default.commands.allow = [ "modern-web-guidance" ];

  agents.instructionText = lib.mkAfter modernWebGuidanceInstructionText;

  agents.commandContext = lib.mkAfter modernWebGuidanceCommandContext;

  home.packages = [ pkgs.local.modern-web-guidance ];
}
