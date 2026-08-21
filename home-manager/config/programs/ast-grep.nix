{ lib, pkgs, ... }:

let
  astGrepInstructionText = lib.strings.trim ''
    - Use the `ast-grep` skill for structural code search and analysis using AST-based patterns.
  '';

  astGrepCommandContext = lib.strings.trim ''
    - `ast-grep` — read-only; no network access, no filesystem modifications, no background daemons
  '';

in

{
  agents.skillSets.ast-grep = "${pkgs.local.ast-grep}/skills";

  agents.permissions.default.commands.allow = [ "ast-grep" ];

  agents.instructionText = lib.mkAfter astGrepInstructionText;

  agents.commandContext = lib.mkAfter astGrepCommandContext;

  home.packages = [ pkgs.local.ast-grep ];
}
