{ config, lib, pkgs, ... }:

let
  cfg = config.programs.jujutsu;

  gpgcfg = config.programs.gpg;

  gitcfg = config.programs.git;
in
{
  programs.jujutsu = {
    enable = true;
    package = pkgs.unstable.jujutsu;

    settings = {
      user = {
        email = gitcfg.settings.user.email;
        name = gitcfg.settings.user.name;
      };

      signing = lib.mkIf gpgcfg.enable {
        behavior = "own";
        backend = "gpg";
        "backend.gpg.program" = "${gpgcfg.package}/bin/gpg";
      };

      ui = {
        conflict-marker-style = "git";
        default-command = "log";
        diff-editor = ":builtin";
        diff-formatter = ":git";
        pager = "${pkgs.delta}/bin/delta";
        show-cryptographic-signatures = gpgcfg.enable;
      };

      snapshot = {
        auto-update-stale = true;
      };

      aliases =
        let
          jjSnapshot = pkgs.writeScriptBin "jj-snapshot" ''
            #!${pkgs.bash}/bin/bash
            ${cfg.package}/bin/jj commit -m  "snapshot: $(${pkgs.coreutils}/bin/date +%s)"
          '';
          jjWorkspaceSync = pkgs.writeScriptBin "jj-workspace-sync" ''
            #!${pkgs.bash}/bin/bash
            set -euo pipefail

            jj_bin="${cfg.package}/bin/jj"
            coreutils_bin="${pkgs.coreutils}/bin"

            workspace_root="$("$jj_bin" root --color=never)"
            repo_link="$workspace_root/.jj/repo"

            if [ -L "$repo_link" ]; then
              repo_dir="$("$coreutils_bin/readlink" -f "$repo_link")"
            elif [ -f "$repo_link" ]; then
              repo_dir="$("$coreutils_bin/cat" "$repo_link")"
            else
              echo "jj: could not resolve repo link at $repo_link" >&2
              exit 1
            fi

            if [ -z "$repo_dir" ]; then
              echo "jj: repo link at $repo_link is empty" >&2
              exit 1
            fi

            canonical_root="$("$coreutils_bin/dirname" "$("$coreutils_bin/dirname" "$repo_dir")")"
            exec "$jj_bin" -R "$canonical_root" workspace update-stale
          '';
        in
        {
          snapshot = [ "util" "exec" "--" "${jjSnapshot}/bin/jj-snapshot" ];
          diff-ls = [ "diff" "--summary" ];
          workspace-sync = [ "util" "exec" "--" "${jjWorkspaceSync}/bin/jj-workspace-sync" ];
        };
    };
  };

  # Shell completion based on
  # https://github.com/martinvonz/jj/blob/0690922ca15ced55e417edab806c982b0cc42b84/docs/install-and-setup.md
  programs.bash = {
    initExtra = ''
      source <(${cfg.package}/bin/jj util completion bash)
    '';
  };

  programs.zsh = {
    initContent = ''
      autoload -U compinit
      compinit
      source <(${cfg.package}/bin/jj util completion zsh)
    '';
  };

  programs.fish = {
    interactiveShellInit = ''
      ${cfg.package}/bin/jj util completion fish | source
    '';

    functions = {
      # https://gist.github.com/hroi/d0dc0e95221af858ee129fd66251897e
      fish_jj_prompt = {
        body = ''
          if not ${cfg.package}/bin/jj root --quiet &>/dev/null
            return 1
          end

          ${cfg.package}/bin/jj log --ignore-working-copy --no-graph --color always -r @ -T '
            surround(" (", ")",
              separate(
                " ",
                bookmarks.join(", "),
                change_id.shortest(),
                commit_id.shortest(),
                if(conflict, "conflict"),
                if(empty, "empty"),
                if(divergent, "divergent"),
                if(hidden, "hidden"),
              )
            )
          '
        '';
      };
    };
  };
}
