{ pkgs, ... }:

{
  agents.permissions.default.commands.allow = [ "bx" ];

  agents.skillSets.brave-search = {
    path = "${pkgs.local.skill-brave-search}/skills";
    prefix = "brave-search";
    skills = [ "bx" ];
  };

  home.packages = [ pkgs.unstable.brave-search-cli ];
}
