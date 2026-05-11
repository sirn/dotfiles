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
      ];
      color = "red";
      model = "sonnet";
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
      model = lib.mkDefault "gpt-5.5";
    };
  };
}
