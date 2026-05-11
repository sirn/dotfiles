{ lib, pkgs, ... }:

let
  syntheticSearchInstructionText = lib.strings.trim ''
    - Use the `synthetic-search` skill for general web research, finding documentation, and current information.
  '';
in
{
  agents.permissions.default.commands.allow = [ "synthetic-search" ];

  agents.instructionText = lib.mkAfter syntheticSearchInstructionText;

  agents.commandContext = lib.mkAfter ''
`synthetic-search QUERY` searches the web (default: JSON). Use `--list` for title+URL per line, `--text` for readable snippets, `--compact` for pipable JSON. Read-only, requires `SYNTHETIC_API_KEY`.
  '';

  agents.skillSets.synthetic-search = {
    path = "${pkgs.local.synthetic-search-cli}/skills";
    skills = [ "synthetic-search" ];
  };

  home.packages = [ pkgs.local.synthetic-search-cli ];
}
