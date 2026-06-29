{ lib, ... }: {
  agents.subagents.oracle = {
    description = "Adjudicates ambiguous, conflicting, or high-impact technical decisions";
    delegateWhen = "An ambiguous, conflicting, or high-impact technical decision needs adjudication.";
    prompt = builtins.readFile ./oracle.md;
    claude-code = {
      allowedTools = [
        "Read"
        "Grep"
        "Glob"
        "WebSearch"
        "WebFetch"
      ];
      color = "yellow";
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
