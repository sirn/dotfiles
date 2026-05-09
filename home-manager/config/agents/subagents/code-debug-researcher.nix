{
  agents.subagents.code-debug-researcher = {
    description = "Debugs issues by researching errors, logs, and known fixes";
    mode = "plan";
    prompt = builtins.readFile ./code-debug-researcher.md;
    claude-code = {
      allowedTools = [
        "Read"
        "Grep"
        "Glob"
        "WebSearch"
        "WebFetch"
        "mcp__context7__resolve-library-id"
        "mcp__context7__query-docs"
      ];
      color = "red";
      model = "sonnet";
    };
  };
}
