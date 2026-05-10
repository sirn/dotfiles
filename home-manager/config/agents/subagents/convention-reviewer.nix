{ lib, ... }:
{
  agents.subagents.convention-reviewer = {
    description = "Meticulous reviewer for coding conventions and consistency";
    mode = "plan";
    prompt = builtins.readFile ./convention-reviewer.md;
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
      model = lib.mkDefault "gpt-5.5";
    };
  };
}
