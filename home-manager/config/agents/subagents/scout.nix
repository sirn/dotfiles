{ lib, ... }: {
  agents.subagents.scout = {
    description = "Maps local code structure, patterns, and relevant files";
    delegateWhen = "Relevant files, conventions, or call paths must be mapped before acting.";
    prompt = builtins.readFile ./scout.md;
    claude-code = {
      allowedTools = [
        "Read"
        "Grep"
        "Glob"
      ];
      color = "blue";
      model = "sonnet";
    };
    pi = {
      tools = [
        "find"
        "grep"
        "ls"
        "read"
      ];
      model = lib.mkDefault "gpt-5.4-mini";
    };
  };
}
