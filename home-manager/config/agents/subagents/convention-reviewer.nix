{
  agents.subagents.convention-reviewer = {
    description = "Meticulous reviewer for coding conventions and consistency";
    mode = "plan";
    prompt = builtins.readFile ./convention-reviewer.md;
    claude-code = {
      allowedTools = [
        "Read"
        "Grep"
        "Glob"
      ];
      color = "blue";
      model = "sonnet";
    };
    opencode = {
      model = "google/gemini-3-flash-preview";
    };
  };
}
