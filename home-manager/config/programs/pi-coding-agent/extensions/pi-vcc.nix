{ pkgs, ... }:

{
  home.file.".pi/agent/extensions/monotykamary-pi-vcc".source = pkgs.local.pi-vcc;

  home.file.".pi/agent/pi-vcc-config.json".text = builtins.toJSON {
    overrideDefaultCompaction = true;
    debug = false;
  };
}
