{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.jujutsu;

  gpgcfg = config.programs.gpg;

  gitcfg = config.programs.git;
in
{
  programs.jujutsu = {
    enable = true;
    package = pkgs.jujutsu;

    settings = {
      user = {
        email = gitcfg.settings.user.email;
        name = gitcfg.settings.user.name;
      };

      signing = lib.mkIf gpgcfg.enable {
        behavior = "own";
        backend = "gpg";
        "backend.gpg.program" = "${gpgcfg.package}/bin/gpg";
        "backend.gpg.allow-expired-keys" = true;
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
        in
        {
          snapshot = [
            "util"
            "exec"
            "--"
            "${jjSnapshot}/bin/jj-snapshot"
          ];
          diff-ls = [
            "diff"
            "--summary"
          ];
        };
    };
  };

  programs.fish = {
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
