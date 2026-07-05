{ pkgs, ... }:

{
  home.file = {
    ".pi/agent/extensions/monotykamary-pi-tool-repair".source = pkgs.local.pi-tool-repair;
  };
}
