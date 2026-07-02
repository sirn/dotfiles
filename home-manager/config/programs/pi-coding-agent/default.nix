{
  config,
  lib,
  pkgs,
  ...
}:

let
  agentsCfg = config.agents;

  wrappedPi = pkgs.writeScriptBin "pi" ''
    #!${pkgs.runtimeShell}
    export PI_SKIP_VERSION_CHECK=1
    exec "${lib.getExe pkgs.local.envWrapper}" \
      -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env" \
      -i "''${XDG_CONFIG_HOME:-$HOME/.config}/sops-nix/secrets/agents/env.local" \
      -- "${lib.getExe pkgs.llm-agents.pi}" "$@"
  '';
in

{
  imports = [
    ./keybindings.nix
    ./extensions/hm-shell-policy.nix
    ./extensions/hm-subagent.nix
    ./extensions/hm-notify-turn-complete.nix
    ./extensions/hm-plan-mode.nix
    ./extensions/hm-goal-mode.nix
    ./extensions/hm-remote-models.nix
    ./extensions/hm-simple-footer.nix
    ./extensions/hm-smart-compact.nix
    ./extensions/pi-vcc.nix
    ./extensions/pi-tool-repair.nix
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
