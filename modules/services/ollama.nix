{ pkgs, lib, ... }:

{
  home.packages = [
    pkgs.ollama
  ];

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

  launchd.agents.ollama = {
    enable = true;
    config = {
      Label = "org.nix-community.home.ollama";
      RunAtLoad = true;
      KeepAlive = true;
      ProgramArguments = [
        "${pkgs.ollama}/bin/ollama"
        "serve"
      ];
    };
  };
}