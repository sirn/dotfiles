{ lib, ... }:
{
  agents.subagents.simplicity-reviewer = {
    description = "Pragmatic reviewer prioritizing simplicity over abstraction";
    mode = "plan";
    prompt = builtins.readFile ./simplicity-reviewer.md;
    claude-code = {
      allowedTools = [
        "Read"
        "Grep"
        "Glob"
      ];
      color = "green";
      model = "sonnet";
    };
    pi = {
      tools = [
        "read"
        "grep"
        "find"
        "ls"
      ];
      model = lib.mkDefault "gpt-5.5";
    };
  };
}
