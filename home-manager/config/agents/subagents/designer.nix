{ lib, ... }:

{
  agents.subagents.designer = {
    description = "Evaluates visual design, layout, accessibility, and consistency with existing UI patterns";
    prompt = builtins.readFile ./designer.md;
    claude-code = {
      allowedTools = [
        "Read"
        "Grep"
        "Glob"
        "WebSearch"
        "WebFetch"
        "Bash"
      ];
      color = "pink";
      model = "sonnet";
    };
    pi = {
      tools = [
        "read"
        "grep"
        "find"
        "ls"
        "bash"
      ];
      model = lib.mkDefault "claude-sonnet-4.6";
    };
  };
}
