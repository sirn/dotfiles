{ pkgs, ... }:

{
  systemd.user.services.languagetool = {
    Install.WantedBy = [ "default.target" ];

    Unit = {
      Description = "LanguageTool server";
      Documentation = "https://github.com/languagetool-org/languagetool";
    };

    Service = {
      ExecStart = "${pkgs.languagetool}/bin/languagetool-http-server";
    };
  };
}
