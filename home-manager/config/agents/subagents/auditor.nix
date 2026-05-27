{ lib, ... }:
{
  agents.subagents.auditor = {
    description = "Final-pass audit for material issues - correctness, security, data loss, and production risks";
    mode = "plan";
    prompt = builtins.readFile ./auditor.md;
    claude-code = {
      allowedTools = [
        "Read"
        "Grep"
        "Glob"
        "WebSearch"
        "WebFetch"
      ];
      color = "magenta";
      model = "opus";
    };
    pi = {
      tools = [
        "read"
        "grep"
        "find"
        "ls"
      ];
      model = lib.mkDefault "claude-opus-4.7";
    };
  };
}
