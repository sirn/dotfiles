{ pkgs, ... }:

{
  security.doas = {
    enable = true;
    extraRules = [
      {
        groups = [ "wheel" ];
        noPass = true;
        keepEnv = false;
      }
    ];
  };
}
