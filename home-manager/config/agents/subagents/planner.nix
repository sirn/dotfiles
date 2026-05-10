{ lib, ... }:
{
  agents.subagents.planner = {
    description = "Designs minimal implementation, architecture, and refactoring plans";
    mode = "plan";
    prompt = builtins.readFile ./planner.md;
    claude-code = {
      allowedTools = [
        "Read"
        "Grep"
        "Glob"
        "WebSearch"
        "WebFetch"
        "mcp__context7__resolve-library-id"
        "mcp__context7__query-docs"
      ];
      color = "orange";
      model = "opus";
    };
    pi = {
      tools = [
        "read"
        "grep"
        "find"
        "ls"
      ];
      model = lib.mkDefault "claude-sonnet-4.6";
    };
  };
}
