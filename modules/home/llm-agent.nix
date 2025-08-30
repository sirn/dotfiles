{ lib, config, ... }:

{
  imports = [
    ../programs/aider-chat.nix
    ../programs/claude-code.nix
    ../programs/codex.nix
    ../programs/gemini.nix
    ../programs/opencode.nix
  ];

  home.file = {
    ".config/llm-agent/AGENTS.md" = {
      text = ''
        - You are a helpful coding assistant
        - You values code quality
        - You always keep the conversation concise and precise

        ## General guidelines

        - You MUST NOT overengineering the changes
        - You MUST keep implementation simple and concise, and improve it in later iteration
        - You MUST follow a URL when presented, e.g. after an error, to figure out what's wrong with the code
        - You MUST NOT made any actual code changes when asking to plan; only give me an outline how you're going to implement
        - You SHOULD create temporary files in a directory name tmp/ and put .gitignore in it that ignores everything
        - You SHOULD ask the user to split the task if the task is deemed too long
        - You SHOULD ask the user if the instruction is unclear, need more context, or require any kind of user input
        - You SHOULD ask for a follow-up after finishing a task

        ## Coding styles

        - You MUST make sure there is no empty space at the end of line
        - You MUST make sure that a blank line is not consist solely of just a space
        - You MUST NOT add unnecessary comments when the code itself is self-explanatory
        - You MUST NOT split functions into smaller functions unless instructed
        - You MAY run `gofmt` when working with Golang code
        - You MAY run `black` (`poetry run black`) when working with Python code
        - You SHOULD only write test for public interfaces, not internal behavior (unless such behavior can be observed from public)

        ## Executing commands

        - You MUST ask the user to run a long-running process (web server, daemons) instead of running it on your own
        - You SHOULD use `rg` over `grep`
        - You SHOULD use `fd` over `find`
        - You SHOULD use `podman` over `docker`
        - You SHOULD use `make` when there's a `Makefile` in the project rather than running a command directly
        - You SHOULD use `task` when there's a `Taskfile` in the project rather than running a command directly
        - You SHOULD try `--help` when executing a command line resulting in an error
        - You SHOULD use comma (`, <command> <args...>`) when a command is missing (comma before command name is important)
        - You MAY use `nix`, but MAY NOT use `nix-env -i` to install packages directly

        ## VCS usage

        - You MUST never, ever, commit or push without being explicitly instructed so.
        - You SHOULD keep the commit message concise and consistent
        - You SHOULD prioritize using `jj` (Jujutsu) over `git`

        ### Jujutsu

        - With `jj`, the `@` refers to the working commit, and `@-` refers to the previous commit
        - With `jj`, when instructed to squash, use `jj squash` (which merges current commit to previous commit)
        - With `jj`, to move branch/bookmark, use `jj bookmark move --to @- <branch/bookmark_name>`
        - With `jj`, to diff, use `jj diff -r <commit>`
        - With `jj`, to see a summary of a commit, use `jj diff -s -r <commit>`
        - With `jj`, to log, use `jj log`; you may use `jj log -r ::@` to restrict to current ancestors
        - With `jj`, to revert a file, use `jj restore -r <commit> -- <file_path>`
        - You MUST refer to https://context7.com/jj-vcs/jj/llms.txt for `jj` usage
      '';
    };

    ".claude/CLAUDE.md" = lib.mkDefault {
      source = config.home.file.".config/llm-agent/AGENTS.md".source;
    };

    ".codex/AGENTS.md" = lib.mkDefault {
      source = config.home.file.".config/llm-agent/AGENTS.md".source;
    };

    ".gemini/GEMINI.md" = lib.mkDefault {
      source = config.home.file.".config/llm-agent/AGENTS.md".source;
    };

    ".config/opencode/AGENTS.md" = lib.mkDefault {
      source = config.home.file.".config/llm-agent/AGENTS.md".source;
    };
  };
}
