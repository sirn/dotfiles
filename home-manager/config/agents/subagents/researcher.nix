{ lib, ... }: {
  agents.subagents.researcher = {
    description = "Finds authoritative docs, APIs, errors, migrations, and advisories";
    delegateWhen = "Authoritative external docs, APIs, errors, advisories, or compatibility are needed.";
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
        "bash"
        "find"
        "grep"
        "ls"
        "read"
      ];
      model = lib.mkDefault "gpt-5.5-mini";
    };
  };
}
