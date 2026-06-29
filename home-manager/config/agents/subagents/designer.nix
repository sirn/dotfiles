{ lib, ... }:

{
  agents.subagents.designer = {
    description = "Evaluates visual design, layout, accessibility, and consistency with existing UI patterns";
    delegateWhen = "Visual/layout/accessibility evaluation or consistency with existing UI.";
    prompt = builtins.readFile ./designer.md;
    claude-code = {
      allowedTools = [
        "Read"
        "Grep"
        "Glob"
        "WebSearch"
        "WebFetch"
        "Bash"
      ];
      color = "pink";
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
      model = lib.mkDefault "claude-sonnet-4.6";
    };
  };
}
