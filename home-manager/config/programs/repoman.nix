{
  config,
  lib,
  pkgs,
  ...
}:

let
  tomlFormat = pkgs.formats.toml { };

  repomanSkillSet = pkgs.writeTextDir "repoman/SKILL.md" ''
    ---
    name: repoman
    type: reference
    description: Reference for using repoman to locate, clone, sync, and manage local repository/workspace directories. Read when navigating or creating repos/workspaces under ~/Dev.
    ---

    ## Overview

    Repoman organizes repositories under host/repo directory layouts and tracks named workspaces.

    Configured roots:

    - `${config.home.homeDirectory}/Dev/src`
    - `${config.home.homeDirectory}/Dev/go/gopath/src`

    Configured workspace roots:

    - `${config.home.homeDirectory}/Dev/workspace`
    - `${config.home.homeDirectory}/Dev/adhoc`

    ## Common Commands

    - `repoman sync` - refresh known repositories/workspaces.
    - `repoman list` - list known repository directories.
    - `repoman clone <url>` - clone into the configured host/repo layout.
    - `repoman clone --backend jj <url>` - clone using Jujutsu.
    - `repoman clone --backend git <url>` - clone using Git.
    - `repoman pull-all` - update repositories on allowed branches.
    - `repoman workspace add <name> <repo-path>` - create or extend a named workspace.
    - `repoman workspace list` - list workspace directories.
    - `repoman workspace list --repo` - list repositories inside workspaces.
    - `repoman workspace delete <name>` - delete a workspace after confirmation.

    ## Directory Layout

    - Source repos: `${config.home.homeDirectory}/Dev/src/<host>/<repo>`
    - Workspaces: `${config.home.homeDirectory}/Dev/workspace/<name>/<repo>`
    - Ad-hoc projects: `${config.home.homeDirectory}/Dev/adhoc/<name>`

    ## Safety

    - Treat workspace deletion as destructive; ask before running `repoman workspace delete` unless the user explicitly requested it.
    - Do not run `repoman pull-all` if unrelated repositories may be modified unless the user asked for a batch update.
    - Stay inside the current project/workspace unless the user explicitly asks to navigate elsewhere.
    - When you only need to locate a repository or workspace, prefer read-only commands: `repoman list` and `repoman workspace list`.
  '';

  repomanInstructionText = lib.strings.trim ''
    - Use `repoman list` and `repoman workspace list` to locate project directories; read the `repoman` skill before clone, batch update, or workspace operations."
  '';

  tmuxcfg = config.programs.tmux;

  fzyCmd = lib.getExe pkgs.fzy;
in
{
  home.packages = with pkgs; [ local.repoman ];

  agents.skillSets.repoman = repomanSkillSet;
  agents.instructionText = lib.mkAfter repomanInstructionText;

  xdg.configFile."repoman/config.toml".source = tomlFormat.generate "repoman-config" {
    roots = [
      "${config.home.homeDirectory}/Dev/src"
      "${config.home.homeDirectory}/Dev/go/gopath/src"
    ];
    workspaces = [
      "${config.home.homeDirectory}/Dev/workspace"
      "${config.home.homeDirectory}/Dev/adhoc"
    ];
  };

  # Allow repoman to clone and manage repos/workspaces from inside the agent sandbox.
  agents.sandbox.extraWritePaths = [
    "${config.home.homeDirectory}/Dev/src"
    "${config.home.homeDirectory}/Dev/go/gopath/src"
    "${config.home.homeDirectory}/Dev/workspace"
    "${config.home.homeDirectory}/Dev/adhoc"
  ];

  programs.zsh.initContent = lib.mkIf config.programs.zsh.enable ''
    # Quickly jump into dev project directory.
    ggd() {
      local dir

      dir=$(repoman list | "${fzyCmd}" -q "$*")
      if [ -z "$dir" ]; then
          return
      fi

      builtin cd "$dir" || return 1
    }

    # Quickly jump into workspace directory.
    ggw() {
      local dir

      dir=$(repoman workspace list | "${fzyCmd}" -q "$*")
      if [ -z "$dir" ]; then
          return
      fi

      builtin cd "$dir" || return 1
    }

    ${lib.optionalString tmuxcfg.enable ''
      # Open a workspace or project directory in a tmux session.
      # Use exec when outside tmux so detaching closes the terminal.
      # Don't exec when inside tmux or the pane will close on switch.
      #
      # Note: tmux_init has matching $TMUX logic - it uses switch-client when
      # inside tmux (exits immediately) and new-session when outside (blocks).
      #
      # We need the conditional exec in both places to get the right behavior:
      # - Outside tmux: exec tmux_init → exec new-session → terminal closes on detach
      # - Inside tmux: run tmux_init → switch-client → return to shell, pane stays open
      #
      ggt() {
        local dir name

        dir=$( { repoman workspace list; repoman list; } | "${fzyCmd}" -q "$*")
        if [ -z "$dir" ]; then
            return
        fi

        name=$(basename "$dir")
        if [ -n "$TMUX" ]; then
          "$HOME/.tmux_init" "$name" "$dir"
        else
          exec "$HOME/.tmux_init" "$name" "$dir"
        fi
      }
    ''}
  '';

  programs.fish.functions = lib.mkIf config.programs.fish.enable (
    {
      ggd = {
        body = ''
          set -l dir (repoman list | ${fzyCmd} -q "$argv")
          if test -z "$dir"
            return
          end
          cd $dir
        '';
      };

      ggw = {
        body = ''
          set -l dir (repoman workspace list | ${fzyCmd} -q "$argv")
          if test -z "$dir"
            return
          end
          cd $dir
        '';
      };
    }
    // lib.optionalAttrs tmuxcfg.enable {
      ggt = {
        body = ''
          # Open a workspace or project directory in a tmux session.
          #
          # Use exec when outside tmux so detaching closes the terminal.
          # Don't exec when inside tmux or the pane will close on switch.
          #
          # Note: tmux_init has matching $TMUX logic - it uses switch-client when
          # inside tmux (exits immediately) and new-session when outside (blocks).
          #
          # We need the conditional exec in both places to get the right behavior:
          # - Outside tmux: exec tmux_init → exec new-session → terminal closes on detach
          # - Inside tmux: run tmux_init → switch-client → return to shell, pane stays open
          #
          set -l dir (begin; repoman workspace list; repoman list; end | ${fzyCmd} -q "$argv")
          if test -z "$dir"
            return
          end
          set -l name (basename $dir)
          if set -q TMUX
            $HOME/.tmux_init $name $dir
          else
            exec $HOME/.tmux_init $name $dir
          end
        '';
      };
    }
  );
}
