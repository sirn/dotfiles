{ lib, ... }: {
  agents.subagents.reviewer = {
    description = "Reviews correctness, security, conventions, simplicity, and quality";
    delegateWhen = "Code/design/plan needs review through a specific lens (correctness/security/convention/simplicity).";
    prompt = builtins.readFile ./reviewer.md;
    claude-code = {
      allowedTools = [
        "Glob"
        "Grep"
        "Read"
        "WebFetch"
        "WebSearch"
      ];
      color = "red";
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
      model = lib.mkDefault "gpt-5.5";
    };
  };
}
