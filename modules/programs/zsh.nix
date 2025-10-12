{ config, lib, pkgs, ... }:

let
  cfg = config.programs.zsh;
in
{
  programs.zsh = {
    enable = true;
    enableVteIntegration = true;

    syntaxHighlighting = {
      enable = true;
    };

    autocd = true;

    history = {
      append = true;
      expireDuplicatesFirst = true;
      ignoreAllDups = true;
      ignoreDups = true;
      save = 10000;
      size = 10000;
    };

    initContent =
      let
        dirJumpCmd =
          let
            fzyCmd = lib.getExe pkgs.fzy;

            fdCmd = lib.getExe pkgs.fd;
          in
          pkgs.writeScript "dir-jump-cmd" ''
            # Change to a subdirectory of the current directory.
            gg() {
              local dir

              dir=$("${fdCmd}" --type d . | "${fzyCmd}" -q "$*")
              if [ -z "$dir" ]; then
                  return
              fi

              builtin cd "$dir" || return 1
            }

            # Quickly jump into dev project directory.
            ggd() {
              local dir

              dir=$($HOME/.dotfiles/bin/pom list | "${fzyCmd}" -q "$*")
              if [ -z "$dir" ]; then
                  return
              fi

              builtin cd "$dir" || return 1
            }

            # Change directory in a project directory.
            ggp() {
              local dir

              if [ ! -d "$HOME/Dropbox/Projects" ]; then
                echo >&2 "No projects directory"
                return 1
              fi

              dir=$("${fdCmd}" --type d . "$HOME/Dropbox/Projects" | "${fzyCmd}" -q "$*")
              if [ -z "$dir" ]; then
                  return
              fi

              builtin cd "$dir" || return 1
            }

            # Navigate to ancestor directories.
            bb() {
              local current_dir dirs dir

              current_dir=$(pwd)
              dirs=""
              while [ "$current_dir" != "/" ]; do
                current_dir=$(dirname "$current_dir")
                dirs="$dirs$current_dir"$'\n'
              done

              if [ -z "$dirs" ]; then
                return
              fi

              dir=$(printf "%s" "$dirs" | "${fzyCmd}" -q "$*")
              if [ -z "$dir" ]; then
                return
              fi

              builtin cd "$dir" || return 1
            }
          '';

        historySetup =
          let
            awkCmd = lib.getExe pkgs.gawk;

            cutCmd = lib.getExe' pkgs.coreutils "cut";

            fzyCmd = lib.getExe pkgs.fzy;
          in
          pkgs.writeScript "history-setup" ''
            __fzy_history() {
              local _selection

              _selection=$(fc -ln -1 0 | "${cutCmd}" -f3- | ${awkCmd} '!seen[$0]++' | ${fzyCmd} -q "$*")
              if [ "$?" = "0" ] || [ -n "$_selection" ]; then
                BUFFER=''${_selection}
                CURSOR=''${#BUFFER}
              fi

              zle redisplay
            }

            zle -N __fzy_history
            bindkey '^R' __fzy_history
          '';

        promptSetup =
          let
            awkCmd = lib.getExe pkgs.gawk;

            gitCmd = lib.getExe config.programs.git.package;

            hostnameCmd = lib.getExe' pkgs.hostname-debian "hostname";

            jjCmd = lib.getExe config.programs.jujutsu.package;

            sedCmd = lib.getExe pkgs.gnused;

            tputCmd = lib.getExe' pkgs.ncurses "tput";
          in
          pkgs.writeScript "prompt-setup" ''
            # Sets up user prompt
            _prompt() {
              if [ "$(id -u)" = "0" ]; then
                print \#
              else
                print \$
              fi
            }

            _prompt_git() {
              local _branch
              _branch=$("${gitCmd}" rev-parse --abbrev-ref HEAD 2>/dev/null)

              if [ -n "$_branch" ]; then
                "${gitCmd}" status --porcelain=v2 2>/dev/null |
                  "${awkCmd}" \
                    -v branch="$_branch" \
                    -v bold="%{$(tput bold 2>/dev/null || true)%}" \
                    -v reset="%{$(tput sgr0 2>/dev/null || true)%}" \
                    -v green="%{$(tput setaf 2 2>/dev/null || true)%}" \
                    -v yellow="%{$(tput setaf 3 2>/dev/null || true)%}" \
                    '
                    BEGIN {
                      changed=0
                      staged=0
                    }
                    /^1 M./ { staged=1 }
                    /^1 .M/ { changed=1 }
                    /^[^1]/ { changed=1 }
                    END {
                      printf "git:%s%s%s", bold, branch, reset
                      if (staged) { printf "%s%s", green, "*" }
                      if (changed) { printf "%s%s", yellow, "*" }
                      printf "%s ", reset
                    }'
              fi
            }

            _prompt_jj() {
              if ! "${jjCmd}" root >/dev/null 2>&1; then
                return
              fi

              local _spec
              _spec=$("${jjCmd}" log --ignore-working-copy --no-graph -r @ -T 'separate(":",
                change_id.shortest(),
                if(empty, "0", "1"),
              )')

              local _changeid=''${_spec%%:*};
              local _changed=''${_spec##''${_changeid}:}

              printf "jj:%s%s%s" \
                "%{$("${tputCmd}" bold 2>/dev/null || true)%}" \
                "$_changeid" \
                "%{$("${tputCmd}" sgr0 2>/dev/null || true)%}"

              if [ "$_changed" = "1" ]; then
                printf "%s*%s" \
                  "%{$("${tputCmd}" setaf 3 2>/dev/null || true)%}" \
                  "%{$("${tputCmd}" sgr0 2>/dev/null || true)%}"
              fi

              printf " "
            }

            _prompt_pwd() {
              pwd | "${sedCmd}" "
            s|^$HOME|~|;                                    # $HOME -> ~
            s|\([^[:punct:]]\)[^/]*/|\1/|g;                 # foo/bar/baz -> f/b/baz
            s|^\(././\)././././.*/\(./[^/]*\)$|\1.../\2|g;  # 1/2/3/4/5/6/7/8/9/10 -> 1/.../9/10
            "
            }

            _prompt_hostname() {
              local short_hostname
              short_hostname=$("${hostnameCmd}" -s)
              printf "%s" "''${short_hostname%%.*}"
            }

            _prompt_last_exit() {
              local last_exit=$?
              local format_code=

              if [ "$last_exit" = 1 ]; then
                format_code=$("${tputCmd}" setab 1 2>/dev/null || true)
              elif [ "$last_exit" != 0 ]; then
                format_code=$("${tputCmd}" setab 3 2>/dev/null || true)
              fi

              if [ -n "$format_code" ]; then
                printf \
                  "%s%s%s " \
                  "%{$format_code%}" \
                  "$last_exit" \
                  "%{$("${tputCmd}" sgr0 2>/dev/null || true)%}"
              fi
            }

            _prompt_nix() {
              local t_bold t_reset t_highlight
              t_bold=$("${tputCmd}" bold 2>/dev/null || true)
              t_reset=$("${tputCmd}" sgr0 2>/dev/null || true)
              t_highlight=$("${tputCmd}" setaf 2 2>/dev/null || true)

              if [ -n "$IN_NIX_SHELL" ]; then
                printf \
                  "%s[%s]%s " \
                  "%{$t_highlight$t_bold%}" \
                  "nix-shell" \
                  "%{$t_reset%}"
              fi
            }

            _prompt_init() {
              setopt promptsubst
              PROMPT="\
            $(_prompt_nix)\
            $(_prompt_hostname) \
            \$(_prompt_pwd) \
            \$(_prompt_git)\
            \$(_prompt_jj)\
            \$(_prompt_last_exit)\
            $(_prompt) "
            }

            _prompt_init
          '';
      in
      ''
        export WORDCHARS="''${WORDCHARS/\//}"
        if [[ $- == *"i"* ]]; then
          export SHELL=${cfg.package}/bin/zsh
        fi

        source "${dirJumpCmd}"

        source "${historySetup}"

        source "${promptSetup}"
      '';
  };
}
