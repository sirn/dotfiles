{ lib, ... }: {
  agents.subagents.auditor = {
    description = "Final-pass audit for material issues - correctness, security, data loss, and production risks";
    delegateWhen = "Final pre-merge or pre-deploy pass for correctness, security, data-loss, or production risk.";
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
        "bash"
        "find"
        "grep"
        "ls"
        "read"
      ];
      model = lib.mkDefault "claude-opus-4.7";
    };
  };
}
