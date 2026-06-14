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

  agents.commandContext = lib.mkAfter ''
    - `lsp` is an LSP client that manages background Language Server sessions.
    - `lsp session start --lsp <name>` starts a background Language Server daemon; the daemon is stopped with `lsp session stop`
    - `lsp session stop --lsp <name>` terminates the Language Server daemon
    - `lsp hover|definition|references|completion --lsp <name> FILE LINE CHAR` are read-only code intelligence queries
    - `lsp diagnostics --lsp <name> [FILE]` returns compiler/analysis diagnostics (read-only)
    - `lsp symbols --lsp <name> [FILE]` returns document/workspace symbol outlines (read-only)
    - `lsp format --lsp <name> FILE` returns formatting text edits (does not modify files on disk; only returns the edit instructions)
    - Only `session start` has a side effect (starting a managed daemon); `session stop` terminates it; all other subcommands are read-only
  '';

  agents.skillSets.lsp = {
    path = "${pkgs.local.lsp-cli}/skills";
    skills = [ "lsp" ];
  };

  home.packages = [ pkgs.local.lsp-cli ];
}
