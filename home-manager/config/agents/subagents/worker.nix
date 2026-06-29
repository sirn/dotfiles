{ lib, ... }: {
  agents.subagents.worker = {
    description = "Implements focused code and configuration changes";
    delegateWhen = "A focused, well-specified code/config change is ready to apply.";
    prompt = builtins.readFile ./worker.md;
    claude-code = {
      allowedTools = [
        "Bash"
        "Edit"
        "Glob"
        "Grep"
        "MultiEdit"
        "Read"
        "Write"
      ];
      color = "green";
      model = "sonnet";
    };
    pi = {
      tools = [
        "bash"
        "edit"
        "find"
        "grep"
        "ls"
        "read"
        "write"
      ];
      model = lib.mkDefault "claude-sonnet-4.6";
    };
  };
}
