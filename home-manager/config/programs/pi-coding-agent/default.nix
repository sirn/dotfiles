{
  config,
  lib,
  pkgs,
  ...
}:

let
  agentsCfg = config.agents;

  wrappedPi = config.agents.sandbox.mkWrapper {
    name = "pi";
    package = pkgs.llm-agents.pi;
    preExports = ''
      export PI_SKIP_VERSION_CHECK=1
    '';
  };
in

{
  imports = [
    ./keybindings.nix
    ./extensions/hm-shell-policy.nix
    ./extensions/hm-notify-turn-complete.nix
    ./extensions/hm-remote-models.nix
    ./extensions/hm-simple-footer.nix
    ./extensions/hm-smart-compact.nix
    ./extensions/pi-vcc.nix
  ];

  programs.pi-coding-agent = {
    enable = true;

    package = wrappedPi;

    settings = {
      quietStartup = true;
      hideThinkingBlock = false;
      retry = {
        maxRetries = 10;
        maxDelayMs = 0;
      };
    };

    instructionText =
      agentsCfg.instructionText
      + "\n\n"
      + lib.strings.trim ''
        ## Editing restriction

        - ~/.pi is managed by Nix. DO NOT EDIT FILES IN ~/.pi DIRECTLY.
      '';
  };

  home.file.".pi/agent/skills".source = config.agents.skillTrees;
}
