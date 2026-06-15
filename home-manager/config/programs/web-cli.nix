{ lib, pkgs, ... }:

let
  webInstructionText = lib.strings.trim ''
    - Use the `web` skill for browser-rendered inspection: one-shot `web html|text|screenshot URL` for post-JS DOM or screenshots; prefer `curl` for static HTML or JSON.
    - Multi-step: pair `web session start --name N` with `--session N` on each `nav`/`click`/`fill`/..., then `web session stop --name N`.
  '';

  webCommandContext = lib.strings.trim ''
    - `web` — `session start` starts a headless Chromium daemon; all other subcommands read-only; one-shot modes auto-close with no persistent state
  '';

in
{
  agents.permissions.default.commands.allow = [ "web" ];

  agents.instructionText = lib.mkAfter webInstructionText;

  agents.commandContext = lib.mkAfter webCommandContext;

  agents.skillSets.web = {
    path = "${pkgs.local.web-cli}/skills";
    skills = [ "web" ];
  };

  home.packages = [ pkgs.local.web-cli ];
}
