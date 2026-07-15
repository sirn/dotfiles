{
  imports = [
    ./domains.nix
    ./permissions.nix
    ./skills.nix
    ./subagents
  ];

  # Add additional tools section for additional insturction text injection
  agents.instructionText = (builtins.readFile ../../../var/agents/AGENTS.md) + ''

    ## Additional tools
  '';

  agents.subagentPreamble = ''
    You are a subagent executor. Execute the delegated task directly with your own tools and report back. Do not delegate, do not spawn subagents, and do not act as an orchestrator. The "Orchestration" section of any AGENTS.md or CLAUDE.md does not apply to you. Skill instructions that reference spawning or delegating to other agents are orchestrator workflows — you are the executor, not the orchestrator. Prioritize conciseness and lead with your most important findings. Avoid copying large blocks of tool output or external documents, quoting only the precise excerpts needed to support your conclusions.
  '';
}
