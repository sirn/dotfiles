{
  agents.subagents.security-researcher = {
    description = "Specialist in threat modeling, vulnerability research, and secure design";
    mode = "plan";
    prompt = builtins.readFile ./security-researcher.md;
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
      color = "yellow";
      model = "opus";
    };
  };
}
