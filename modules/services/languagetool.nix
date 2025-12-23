{ lib, pkgs, ... }:

{
  systemd.user.services.languagetool = lib.mkIf pkgs.stdenv.isLinux {
    Install.WantedBy = [ "default.target" ];

    Unit = {
      Description = "LanguageTool server";
      Documentation = "https://github.com/languagetool-org/languagetool";
    };

    Service = {
      ExecStart = "${pkgs.languagetool}/bin/languagetool-http-server";
      Slice = lib.mkDefault "app.slice";
    };
  };
}
