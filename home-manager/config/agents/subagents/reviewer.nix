{ lib, ... }: {
  agents.subagents.reviewer = {
    description = "Reviews correctness, security, conventions, simplicity, and quality";
    delegateWhen = "Code/design/plan needs review through a specific lens (correctness/security/convention/simplicity).";
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
