{ lib, ... }: {
  agents.subagents.scout = {
    description = "Maps local code structure, patterns, and relevant files";
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
        "read"
        "grep"
        "find"
        "ls"
      ];
      model = lib.mkDefault "gpt-5.4-mini";
    };
  };
}
