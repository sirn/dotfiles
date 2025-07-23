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

      signing = lib.mkIf config.programs.gpg.enable {
        behavior = "own";
        backend = "gpg";
        "backend.gpg.program" = "${config.programs.gpg.package}/bin/gpg";
      };

      ui = {
        default-command = "log";
        diff-formatter = ":git";
        diff-editor = ":builtin";
        pager = "${pkgs.delta}/bin/delta";
        "show-cryptographic-signatures" = config.programs.gpg.enable;
      };
    };
  };

  # Fix for 0.29.0 deprecating ~/Library/Application Support on Darwin
  # Removable after home-manager >= 25.05
  home.file = lib.mkIf pkgs.stdenv.isDarwin {
    "Library/Application Support/jj/config.toml" = {
      enable = false;
    };

    "${config.xdg.configHome}/jj/config.toml" = {
      source = (pkgs.formats.toml { }).generate
        "jujutsu-config"
        config.programs.jujutsu.settings;
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
