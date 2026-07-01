{ lib, ... }: {
  agents.subagents.planner = {
    description = "Designs minimal implementation, architecture, and refactoring plans";
    delegateWhen = "Implementation/refactor/migration needs sequenced steps with tradeoffs and risks.";
    prompt = builtins.readFile ./planner.md;
    claude-code = {
      allowedTools = [
        "Read"
        "Grep"
        "Glob"
        "WebSearch"
        "WebFetch"
      ];
      color = "orange";
      model = "opus";
    };
    pi = {
      tools = [
        "bash"
        "find"
        "grep"
        "ls"
        "read"
      ];
      model = lib.mkDefault "claude-opus-4.8";
    };
  };
}
