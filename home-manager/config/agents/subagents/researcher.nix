{ lib, ... }:
{
  agents.subagents.researcher = {
    description = "Finds authoritative docs, APIs, errors, migrations, and advisories";
    mode = "plan";
    prompt = builtins.readFile ./researcher.md;
    claude-code = {
      allowedTools = [
        "Read"
        "Grep"
        "Glob"
        "WebSearch"
        "WebFetch"
      ];
      color = "purple";
      model = "sonnet";
    };
    pi = {
      tools = [
        "read"
        "grep"
        "find"
        "ls"
        "bash"
      ];
      model = lib.mkDefault "gpt-5.5";
    };
  };
}
