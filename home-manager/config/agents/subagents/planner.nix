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
      ];
      color = "orange";
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
      model = lib.mkDefault "claude-sonnet-4.6";
    };
  };
}
