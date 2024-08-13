{ config, pkgs, ... }:

let
  inherit (config.home) homeDirectory;
in
{
  home.packages = with pkgs; [
    gitAndTools.git-crypt
  ];

  programs.git = {
    enable = true;

    userName = "Sirn Thanabulpong";
    userEmail = "sirn@ogsite.net";

    lfs.enable = true;

    extraConfig = {
      branch.autoSetupMerge = true;
      color.ui = "auto";
      core.quotepath = false;
      init.defaultBranch = "main";
      pull.rebase = true;
      push.default = "nothing";
      status.branch = true;
      status.short = true;
      submodule.fetchJobs = 8;
      protocol.file.allow = "always";

      pom.root = [
        "${homeDirectory}/Dev/src"
        "${homeDirectory}/Dev/workspace"
        "${homeDirectory}/Dev/go/gopath/src"
      ];

      url = {
        "ssh://git@github.com/".insteadOf = "https://github.com/";
        "ssh://git@gitlab.com/".insteadOf = "https://gitlab.com/";
        "ssh://git@git.sr.ht/".insteadOf = "https://git.sr.ht/";
      };

      safe.directory = [
        "/etc/nixos"
      ];
    };

    includes = [
      { path = "${homeDirectory}/.dotpriv/etc/git/gitconfig"; }
      { path = "${homeDirectory}/.config/git/config_local"; }
    ];

    ignores = [
      # Editor files
      "*~"
      "*.swp"

      # System files
      ".DS_Store*"
      "ehthumbs.db"
      "Icon?"
      "Thumbs.db"

      # Temp files
      "dump.rdb"

      # Per-project files
      "*.iml"
      "*.pyc"
      ".dev/"
      ".direnv/"
      ".dir-locals.el"
      ".env"
      ".envrc"
      ".gems/"
      ".generators/"
      ".idea/"
      ".mypy_cache/"
      ".tmp/"
      ".vagrant/"
      "Vagrantfile"
      "cache/"
      "out/"

      # User files
      ".lspconfig"
      "stdout"
    ];
  };
}
