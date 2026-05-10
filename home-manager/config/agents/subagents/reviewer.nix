{ lib, ... }:
{
  agents.subagents.reviewer = {
    description = "Reviews correctness, security, conventions, simplicity, and quality";
    mode = "plan";
    prompt = builtins.readFile ./reviewer.md;
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
      color = "red";
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
