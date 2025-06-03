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

  home.file = {
    ".claude/CLAUDE.md" = lib.mkDefault {
      text = ''
        # CLAUDE.md

        This file provides guidance to Claude Code (claude.ai/code) when working with code on this machine.

        ## General guidelines

        - When web search is needed, prefer Brave Search
        - Try to do things in batch, e.g. when doing a mass replace, use `sed` instead of going through each file
        - When presented with a URL after an error, do follow that URL to find out the details
        - When creating temporary files, use /tmp

        ## Coding styles

        - Make sure no empty space at the end of line
        - Make sure that an empty line is not consist of just space
        - Do not add unnecessary comments when the code itself is self-explanatory
        - When working with Golang code, always run `gofmt`

        ## Executing commands

        - Prefer `rg` over `grep`
        - Prefer `podman` over `docker`
        - If there's a Makefile in a project, prefer to use make rather than running the command directly
        - When encountered a command line error, first consult manual with `--help` or `man`
        - When using sed, take account the difference between `-i` usage in macOS and Linux
        - You have `nix` at your disposal; when a command is missing, try using comma `, <command> <args...>`

        ## VCS usage

        - Never, ever, push on your own!
        - Never commit unless explicitly instructed so
        - Commit message must be concise and consistent with my own commit message

        ### In Jujutsu (jj) project

        When Jujutsu is used in a project, i.e. `.jj` is present:

        - Use `jj` instead of `git`
        - In `jj`, the `@` refers to the working commit, and `@-` refers to the previous commit
        - When instructed to squash, use `jj squash` (which merges current commit to previous commit)
        - To move branch/bookmark, use `jj bookmark move --to @- <branch/bookmark_name>`
        - To diff, use `jj diff -r <commit>`
        - To log, use `jj log`; you may use `jj log -r ::@` to restrict to current ancestors
        - Refer to https://context7.com/jj-vcs/jj/llms.txt for usage
      '';
    };
  };
}
