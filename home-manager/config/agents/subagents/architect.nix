{ lib, ... }:

{
  agents.subagents.architect = {
    description = "Analyzes module boundaries, ownership, and structural design for minimal architecture decisions";
    delegateWhen = "A change crosses module/ownership boundaries or needs new interfaces/data-flow decisions.";
    prompt = builtins.readFile ./architect.md;
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
      model = lib.mkDefault "claude-fable-5";
    };
  };
}
