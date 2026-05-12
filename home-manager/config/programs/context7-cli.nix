{ lib, pkgs, ... }:

let
  context7InstructionText = lib.strings.trim ''
    - Use the `context7` skill for retrieving up-to-date library documentation (React, Python stdlib, Rust, etc.) BEFORE implementing or writing code.
  '';
in
{
  agents.permissions.default.commands.allow = [ "context7" ];

  agents.instructionText = lib.mkAfter context7InstructionText;

  agents.commandContext = lib.mkAfter ''
    `context7 /owner/repo -q "query"` retrieves curated library documentation. Use `--output=text` for markdown, `--output=compact` for pipable JSON. Pin versions with `-v`. Read-only.
  '';

  agents.skillSets.context7 = {
    path = "${pkgs.local.context7-cli}/skills";
    skills = [ "context7" ];
  };

  home.packages = [ pkgs.local.context7-cli ];
}
