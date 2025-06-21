{ lib, pkgs, ... }:

{
  home.packages = [
    (pkgs.local.claude-code.overrideDerivation (attrs: {
      # MCP typically requires npx/uvx/etc.; let's make sure that
      # nodejs/uv is always available to claude
      postInstall = attrs.postInstall + ''
        wrapProgram $out/bin/claude \
          --prefix PATH : ${pkgs.nodejs}/bin \
          --prefix PATH : ${pkgs.uv}/bin
      '';
    }))
  ];

  programs.git = {
    ignores = [
      "CLAUDE.local.md"
      ".claude/"
    ];
  };

  xdg.configFile = {
    "claude/CLAUDE.md" = lib.mkDefault {
      text = ''
        # CLAUDE.md

        This file provides guidance to Claude Code (claude.ai/code) when working with code on this machine.

        ## General guidelines

        - You MUST keep the conversation concise and precise
        - You MUST to use Brave Search over Web Search
        - You MUST follow a URL when presented, e.g. after an error, to figure out what's wrong with the code
        - You SHOULD create temporary files in a directory name tmp/
        - You SHOULD ask the user if the instruction is unclear, need more context, or require any kind of user input

        ## Coding styles

        - You MUST make sure there is no empty space at the end of line
        - You MUST make sure that a blank line is not consist solely of just a space
        - You SHOULD NOT add unnecessary comments when the code itself is self-explanatory
        - You SHOULD run `gofmt` when working with Golang code
        - You SHOULD run `black` (`poetry run black`) when working with Python code

        ## Executing commands

        - You SHOULD use `rg` over `grep`
        - You SHOULD use `podman` over `docker`
        - You SHOULD use `make` when there's a `Makefile` in the project rather than running a command directly
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
  };
}
