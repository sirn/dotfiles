{ lib, ... }:
{
  agents.subagents.worker = {
    description = "Implements focused code and configuration changes";
    prompt = builtins.readFile ./worker.md;
    claude-code = {
      allowedTools = [
        "Read"
        "Grep"
        "Glob"
        "Edit"
        "MultiEdit"
        "Write"
        "Bash"
      ];
      color = "green";
      model = "sonnet";
    };
    pi = {
      tools = [
        "read"
        "grep"
        "find"
        "ls"
        "edit"
        "write"
        "bash"
      ];
      model = lib.mkDefault "claude-sonnet-4.6";
    };
  };
}
