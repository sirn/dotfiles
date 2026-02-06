{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    local.repoman
  ];

  programs.git.settings = {
    repoman.root = [
      "${config.home.homeDirectory}/Dev/src"
      "${config.home.homeDirectory}/Dev/go/gopath/src"
    ];

    repoman.workspace = [
      "${config.home.homeDirectory}/Dev/workspace"
      "${config.home.homeDirectory}/Dev/adhoc"
    ];
  };
}
