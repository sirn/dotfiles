{ config, lib, ... }:

let
  cfg = config.programs.gpg;
in
{
  programs.gpg = {
    enable = lib.mkDefault true;

    settings = {
      default-key = "0x65513F82ACB5F854";
    };
  };

  programs.fish.interactiveShellInit = ''
    function __gpg_updatestartuptty --on-event fish_preexec
      ${cfg.package}/bin/gpg-connect-agent --no-autostart updatestartuptty /bye >/dev/null 2>&1
    end
  '';

  programs.zsh.initExtra = ''
    __gpg_updatestartuptty {
      ${cfg.package}/bin/gpg-connect-agent --no-autostart updatestartuptty /bye >/dev/null 2>&1
    }

    preexec_functions+=(__gpg_updatestartuptty)
  '';
}
