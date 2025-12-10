{ pkgs, lib, ... }:

{
  # Not using services.ssh-agent.enable here since we need to configure this
  # on both Darwin and Linux; and services.ssh-agent.enable has an assertion
  # against non-Linux
  systemd.user.services.ssh-agent = {
    Install.WantedBy = [ "default.target" ];

    Unit = {
      Description = "SSH authentication agent";
      Documentation = "man:ssh-agent(1)";
    };

    Service = {
      ExecStart = "${pkgs.openssh}/bin/ssh-agent -D -a %t/ssh-agent";
      Slice = lib.mkDefault "session.slice";
    };
  };

  launchd.agents.ssh-agent = {
    enable = true;
    config = {
      RunAtLoad = true;
      KeepAlive = true;
      ProgramArguments = [
        "/bin/sh"
        "-l"
        "-c"
        ''
          ${pkgs.openssh}/bin/ssh-agent -D -a ''${XDG_RUNTIME_DIR:-$XDG_CACHE_HOME}/ssh-agent
        ''
      ];
    };
  };

  # No check for -z SSH_AUTH_SOCK; OrbStack for instance try to set this
  # to that of the host, of which we want to avoid sharing the agent
  home.sessionVariablesExtra = ''
    export SSH_AUTH_SOCK=''${XDG_RUNTIME_DIR:-$XDG_CACHE_HOME}/ssh-agent
  '';
}
