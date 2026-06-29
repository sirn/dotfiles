{ lib, ... }: {
  agents.subagents.writer = {
    description = "Writes clear technical documentation, READMEs, and guides with minimal jargon";
    delegateWhen = "Technical docs, READMEs, guides, or doc comments need writing or editing.";
    prompt = builtins.readFile ./writer.md;
    claude-code = {
      allowedTools = [
        "Edit"
        "Glob"
        "Grep"
        "Read"
        "Write"
      ];
      color = "cyan";
      model = "sonnet";
    };
    pi = {
      tools = [
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
