{ lib, ... }: {
  agents.subagents.writer = {
    description = "Writes clear technical documentation, READMEs, and guides with minimal jargon";
    delegateWhen = "Technical docs, READMEs, guides, or doc comments need writing or editing.";
    prompt = builtins.readFile ./writer.md;
    claude-code = {
      allowedTools = [
        "Read"
        "Grep"
        "Glob"
        "Edit"
        "Write"
      ];
      color = "cyan";
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
      ];
      model = lib.mkDefault "claude-sonnet-4.6";
    };
  };
}
