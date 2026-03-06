{ config, pkgs, ... }:

let
  tomlFormat = pkgs.formats.toml { };
in
{
  home.packages = with pkgs; [
    local.repoman
  ];

  xdg.configFile."repoman/config.toml".source = tomlFormat.generate "repoman-config" {
    roots = [
      "${config.home.homeDirectory}/Dev/src"
      "${config.home.homeDirectory}/Dev/go/gopath/src"
    ];
    workspaces = [
      "${config.home.homeDirectory}/Dev/workspace"
      "${config.home.homeDirectory}/Dev/adhoc"
    ];
  };
}
