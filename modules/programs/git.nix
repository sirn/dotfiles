{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    gitAndTools.git-crypt
  ];

  programs.git = {
    enable = true;

    userName = lib.mkDefault "Sirn Thanabulpong";
    userEmail = lib.mkDefault "sirn@ogsite.net";

    lfs.enable = true;

    signing = lib.mkIf config.programs.gpg.enable {
      signByDefault = true;
      signer = "${config.programs.gpg.package}/bin/gpg";
      key = config.programs.gpg.settings.default-key;
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

      pom.root = [
        "${config.home.homeDirectory}/Dev/src"
        "${config.home.homeDirectory}/Dev/workspace"
        "${config.home.homeDirectory}/Dev/go/gopath/src"
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
      { path = "${config.home.homeDirectory}/.config/git/config_local"; }
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
      "*.local.md"
      ".lspconfig"
      "pyrightconfig.json"
      "stdout"
      "tmp/"
    ];
  };
}
