{ config, pkgs, lib, ... }:

let
  gpgcfg = config.programs.gpg;
in
{
  home.packages = with pkgs; [
    git-crypt
  ];

  programs.git = {
    enable = true;

    lfs.enable = true;

    signing = lib.mkIf gpgcfg.enable {
      signByDefault = true;
      signer = "${gpgcfg.package}/bin/gpg";
      key = gpgcfg.settings.default-key;
    };

    settings = {
      branch.autoSetupMerge = true;
      color.ui = "auto";
      core.quotepath = false;
      init.defaultBranch = "main";
      protocol.file.allow = "always";
      pull.rebase = true;
      push.default = "nothing";
      push.gpgSign = "if-asked";
      status.branch = true;
      status.short = true;
      submodule.fetchJobs = 8;
      user.email = lib.mkDefault "sirn@ogsite.net";
      user.name = lib.mkDefault "Sirn Thanabulpong";

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
