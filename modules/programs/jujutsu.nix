{ config, lib, pkgs, ... }:

{
  programs.jujutsu = {
    enable = true;
    package = pkgs.unstable.jujutsu;

    settings = {
      user = {
        email = config.programs.git.userEmail;
        name = config.programs.git.userName;
      };

      ui = {
        default-command = "log";
        diff-editor = ":builtin";
        pager = "${pkgs.less}/bin/less -FRX";
      };
    };
  };

  # Shell completion based on
  # https://github.com/martinvonz/jj/blob/0690922ca15ced55e417edab806c982b0cc42b84/docs/install-and-setup.md
  programs.bash = {
    initExtra = ''
      source <(${config.programs.jujutsu.package}/bin/jj util completion bash)
    '';
  };

  programs.zsh = {
    initExtra = ''
      autoload -U compinit
      compinit
      source <(${config.programs.jujutsu.package}/bin/jj util completion zsh)
    '';
  };

  programs.fish = {
    interactiveShellInit = ''
      ${config.programs.jujutsu.package}/bin/jj util completion fish | source
    '';

    functions = {
      # https://gist.github.com/hroi/d0dc0e95221af858ee129fd66251897e
      fish_jj_prompt = {
        body = ''
          if not ${config.programs.jujutsu.package}/bin/jj root --quiet &>/dev/null
            return 1
          end

          ${config.programs.jujutsu.package}/bin/jj log --ignore-working-copy --no-graph --color always -r @ -T '
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
