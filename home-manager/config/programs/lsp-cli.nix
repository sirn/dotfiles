{ lib, pkgs, ... }:

let
  lspInstructionText = lib.strings.trim ''
    - Use the `lsp` skill for semantic code information.
      - Start with `lsp session start --lsp <name>`.
      - Stop with `lsp session stop --lsp <name>`.
      - Use `grep` or `rg` for text search.
      - Use `lsp` for types, definitions, references, and diagnostics.
      - Use `lsp` for completion, symbols, and formatting.
  '';

  lspCommandContext = lib.strings.trim ''
    - `lsp` — `session start` starts a background LSP daemon; `session stop` terminates it; all other subcommands read-only
  '';

in
{
  agents.permissions.default.commands.allow = [ "lsp" ];

  agents.instructionText = lib.mkAfter lspInstructionText;

  agents.commandContext = lib.mkAfter lspCommandContext;

  agents.skillSets.lsp = {
    path = "${pkgs.local.lsp-cli}/skills";
    skills = [ "lsp" ];
  };

  home.packages = [ pkgs.local.lsp-cli ];
}
