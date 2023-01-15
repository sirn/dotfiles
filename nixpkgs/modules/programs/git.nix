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

    signing = {
      key = "8EB047858549B60F2099C9F54AFE17C811D18D20";
      signByDefault = true;
    };

    extraConfig = {
      branch.autoSetupMerge = true;
      color.ui = "auto";
      core.quotepath = false;
      init.defaultBranch = "main";
      pull.rebase = true;
      push.default = "nothing";
      push.gpgSign = "if-asked";
      status.branch = true;
      status.short = true;
      submodule.fetchJobs = 8;
      protocol.file.allow = "always";

      ghq.root = [
        "${homeDirectory}/Dev/src"
        "${homeDirectory}/Dev/workspace"
        "${homeDirectory}/Dev/go/gopath/src"
      ];

      url = {
        "ssh://git@github.com/".insteadOf = "https://github.com/";
        "ssh://git@gitlab.com/".insteadOf = "https://gitlab.com/";
        "ssh://git@git.sr.ht/".insteadOf = "https://git.sr.ht/";
      };
    };

    includes = [
      { path = "${homeDirectory}/.dotpriv/etc/git/gitconfig"; }
      { path = "${homeDirectory}/.gitconfig_local"; }
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
