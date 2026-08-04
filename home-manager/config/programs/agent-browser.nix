{
  config,
  lib,
  pkgs,
  ...
}:

let
  agentBrowserInstructionText = lib.strings.trim ''
    - Use the `agent-browser` skill for browser automation.
      - Use it to navigate, click, fill, take screenshots, and extract data.
      - Use snapshots with `@eN` references.
      - Use `eval` to run JavaScript.
      - Prefer `curl` for static HTML and JSON.
      - Before the first command, run `agent-browser skills get core`.
      - Use `--full` when you need the full reference.
      - The browser stays open between commands.
      - Run `agent-browser close` when finished.
      - Run `agent-browser close --all` to close all sessions.
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

  agents.sandbox.extraWritePaths = [ "${config.home.homeDirectory}/.agent-browser" ];

  # Chrome's internal sandbox conflicts with the outer Seatbelt jail.
  # --no-sandbox is safe here: Seatbelt provides filesystem isolation.
  agents.sandbox.extraEnv.AGENT_BROWSER_ARGS = "--no-sandbox";
}
