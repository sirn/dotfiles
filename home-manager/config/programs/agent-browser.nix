{ lib, pkgs, ... }:

let
  agentBrowserInstructionText = lib.strings.trim ''
    - Use the `agent-browser` skill for browser automation: navigate, snapshot (`@eN` refs), click, fill, screenshot, eval, and extract data. Prefer `curl` for static HTML and JSON.
    - Load the version-matched usage guide before any command: `agent-browser skills get core` (or `--full` for the full reference).
    - The browser stays running across commands; call `agent-browser close` (or `close --all`) when finished.
  '';

  agentBrowserCommandContext = lib.strings.trim ''
    - `agent-browser` — drives a Chrome session over CDP; `open` launches Chrome and keeps it running across commands; `click`/`fill`/`type`/`press` mutate the page, `snapshot`/`get`/`screenshot` read state, `eval` runs arbitrary JS; `close [--all]` terminates the session
  '';

in
{
  agents.permissions.default.commands.allow = [ "agent-browser" ];

  agents.instructionText = lib.mkAfter agentBrowserInstructionText;

  agents.commandContext = lib.mkAfter agentBrowserCommandContext;

  agents.skillSets.agent-browser = {
    path = "${pkgs.llm-agents.agent-browser}/share/agent-browser/skills";
    skills = [ "agent-browser" ];
  };

  home.packages = [ pkgs.llm-agents.agent-browser ];
}
