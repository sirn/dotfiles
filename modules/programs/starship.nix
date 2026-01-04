{ config, lib, pkgs, ... }:

let
  cfg = config.programs.starship;

  jjcfg = config.programs.jujutsu;
in
{
  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    enableFishIntegration = true;

    settings = {
      format = lib.concatStrings [
        "$hostname"
        "$directory"
        "$git_branch"
        "$git_status"
        (lib.optionalString jjcfg.enable "$custom")
        "$nix_shell"
        "$cmd_duration"
        "$line_break"
        "$status"
        "$character"
      ];

      hostname = {
        ssh_only = false;
        format = "[$hostname]($style) ";
        style = "bright-white";
      };

      directory = {
        truncate_to_repo = false;
        fish_style_pwd_dir_length = 1;
        style = "cyan";
      };

      git_branch = {
        format = "git:[$branch]($style)";
        style = "purple";
      };

      git_status = {
        format = "[($conflicted$untracked$modified$staged$renamed$deleted)]($style) ";
        style = "purple";
        modified = "*";
      };

      git_state = {
        disabled = true;
      };

      custom.jj = lib.mkIf jjcfg.enable {
        command = ''
          ${lib.getExe jjcfg.package} log --ignore-working-copy --no-graph -r @ -T 'separate("", change_id.shortest(), if(!empty, "*"))' 2>/dev/null
        '';
        when = "${lib.getExe jjcfg.package} root --quiet";
        format = "jj:[$output]($style) ";
        style = "purple";
      };

      nix_shell = {
        format = "nix-shell:[$state]($style) ";
        style = "green";
      };

      status = {
        disabled = false;
        format = "[$status]($style) ";
        style = "red";
      };

      character = {
        success_symbol = "[\\$](bold white)";
        error_symbol = "[\\$](bold white)";
      };
    };
  };
}
