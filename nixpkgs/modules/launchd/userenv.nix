{ config, pkgs, ... }:

{
  launchd.agents.userenv = {
    enable = true;
    config = {
      RunAtLoad = true;
      ProgramArguments = [
        "/bin/sh"
        "-l"
        "-c"
        "gpg-connect-agent /bye; launchctl unsetenv SSH_AGENT_PID; launchctl setenv SSH_AUTH_SOCK $SSH_AUTH_SOCK"
      ];
    };
  };
}
