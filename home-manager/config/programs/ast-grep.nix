{ pkgs, ... }:

{
  agents.skillSets.ast-grep = "${pkgs.local.skill-ast-grep}/skills";

  agents.permissions.default.commands.allow = [ "ast-grep" ];

  home.packages = [ pkgs.ast-grep ];
}
