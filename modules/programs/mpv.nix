{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (builtins) typeOf stringLength;
  inherit (pkgs.stdenv) isDarwin isLinux;

  # Copied from home-manager/modules/programs/mpv.nix
  renderOption = with lib; option:
    rec {
      int = toString option;
      float = int;
      bool = lib.hm.booleans.yesNo option;
      string = option;
    }.${typeOf option};

  renderOptionValue = with lib; value:
    let
      rendered = renderOption value;
      length = toString (stringLength rendered);
    in
    "%${length}%${rendered}";

  renderOptions = with lib; generators.toKeyValue {
    mkKeyValue = generators.mkKeyValueDefault { mkValueString = renderOptionValue; } "=";
    listsAsDuplicateKeys = true;
  };
in
{
  programs.mpv = {
    # Install via Nix on NixOS and Darwin
    #
    # GPU drivers does not work when Home Manager is only managing user
    # home and not the entire system (and vo=xv is kinda bad).
    enable = isDarwin || (isLinux && config.machine.isNixOS);
    defaultProfiles = [ "gpu-hq" ];
    config = {
      hwdec = "auto";
      af = "lavfi=[loudnorm=I=-18:TP=-1.5:LRA=14]";
      vo = "gpu";
    };
  };

  xdg = mkIf (isLinux && !config.machine.isNixOS) {
    configFile = {
      "mpv/mpv.conf" = {
        text = with lib;
          let
            cfg = config.programs.mpv;
          in
          ''
            ${optionalString
              (cfg.defaultProfiles != [ ])
              (renderOptions { profile = concatStringsSep "," cfg.defaultProfiles; })}
            ${optionalString (cfg.config != { }) (renderOptions cfg.config)}
            ${optionalString (cfg.profiles != { }) (renderOptions cfg.profiles)}
          '';
      };
    };
  };
}
