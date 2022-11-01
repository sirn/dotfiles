{ config, lib, pkgs, ... }:

let
  inherit (builtins) typeOf stringLength;
  inherit (pkgs.stdenv) isDarwin isLinux;
  inherit (lib) mkIf;

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
mkIf config.machine.gui.enable {
  programs.mpv = {
    # Only enable mpv for Darwin since I can't figure out how to make
    # GPU drivers work when Home Manager is only managing user home
    # and not the entire system (and vo=xv is kinda bad).
    #
    # Only Linux this is probably better managed via Flatpak.
    enable = config.machine.gui.enable && isDarwin;
    defaultProfiles = [ "gpu-hq" ];
    config = {
      hwdec = "auto";
      af = "lavfi=[loudnorm=I=-18:TP=-1.5:LRA=14]";
      vo = "gpu";
    };
  };

  machine.flatpak = mkIf (isLinux && config.machine.flatpak.enable) {
    applications = [
      "io.mpv.Mpv"
    ];
  };

  xdg = mkIf (isLinux && config.machine.flatpak.enable) {
    dataFile = {
      "flatpak/overrides/io.mpv.Mpv" = {
        text = ''
          [Context]
          filesystems=xdg-config/mpv
        '';
      };
    };

    configFile = mkIf (config.machine.gui.enable) {
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
