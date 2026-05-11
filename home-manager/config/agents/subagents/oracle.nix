{ lib, ... }:
{
  agents.subagents.oracle = {
    description = "Adjudicates ambiguous, conflicting, or high-impact technical decisions";
    mode = "plan";
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
    gemini = {
      tools = [
        "read_file"
        "grep_search"
        "glob"
        "google_web_search"
        "web_fetch"
        "activate_skill"
      ];
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
