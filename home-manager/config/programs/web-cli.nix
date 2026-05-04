{ lib, pkgs, ... }:

let
  webInstructionText = lib.strings.trim ''
    - Use the `web` skill for browser-rendered inspection: one-shot `web html|text|screenshot URL` for post-JS DOM or screenshots; prefer `curl` for static HTML or JSON.
    - Multi-step: pair `web session start --name N` with `--session N` on each `nav`/`click`/`fill`/..., then `web session stop --name N`.
  '';
in
{
  agents.permissions.default.commands.allow = [ "web" ];

  agents.instructionText = lib.mkAfter webInstructionText;

  agents.commandContext = lib.mkAfter ''
    `web` is a headless Chromium CLI for browser-based inspection.
    - `web session start --name <name>` starts a headless Chromium process; the process is stopped with `web session stop`
    - `web session stop --name <name>` terminates the browser process
    - `web nav --session <name> URL` navigates the browser (read-only, no filesystem side effects)
    - `web html|text|screenshot|pdf --session <name>` are read-only content extraction commands
    - `web eval --session <name> EXPR` evaluates JavaScript in the browser (no filesystem side effects)
    - `web links|console|network --session <name>` are read-only inspection commands
    - One-shot modes (`web html|text|screenshot|pdf URL`) open and close a temporary browser with no persistent state
    - Only `session start` has a side effect (starting a managed browser process); `session stop` terminates it; all other subcommands are read-only
  '';

  agents.skillSets.web = {
    path = "${pkgs.local.web-cli}/skills";
    skills = [ "web" ];
  };

  home.packages = [ pkgs.local.web-cli ];
}
