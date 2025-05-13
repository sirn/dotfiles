{ pkgs, ... }:

{
  systemd.user.services.ollama = {
    Install.WantedBy = [ "default.target" ];

    Unit = {
      Description = "Ollama AI model server";
      Documentation = "https://github.com/ollama/ollama";
    };

    Service = {
      ExecStart = "${pkgs.ollama}/bin/ollama serve";
      Environment = "HOME=%h";
      Restart = "on-failure";
    };
  };
}