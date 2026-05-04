{ lib, pkgs, ... }:

let
  lspInstructionText = lib.strings.trim ''
    - Use the `lsp` skill for semantic code intelligence: hover, definition, references, diagnostics, completion, symbols, formatting.
    - Always start a session first: `lsp session start --lsp <name>`, then query, then stop: `lsp session stop --lsp <name>`.
    - Prefer `grep`/`rg` for text search; use `lsp` for type information, definitions, and compiler diagnostics.
  '';
in
{
  agents.permissions.default.commands.allow = [ "lsp" ];

  agents.instructionText = lib.mkAfter lspInstructionText;
  agents.skillSets.lsp = {
    path = "${pkgs.local.lsp-cli}/skills";
    skills = [ "lsp" ];
  };

  home.packages = [ pkgs.local.lsp-cli ];
}
