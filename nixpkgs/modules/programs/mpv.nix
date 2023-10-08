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
mkIf config.machine.gui.enable {
  programs.mpv = {
    # Install via Nix on Darwin or NixOS; install via Flatpak otherwise.
    #
    # GPU drivers does not work when Home Manager is only managing user
    # home and not the entire system (and vo=xv is kinda bad).
    enable = isDarwin || config.machine.nixos.enable;
    defaultProfiles = [ "gpu-hq" ];
    config = {
      hwdec = "auto";
      af = "lavfi=[loudnorm=I=-18:TP=-1.5:LRA=14]";
      vo = "gpu";
    };
  };

  flatpak.applications = mkIf (isLinux && config.flatpak.enable) {
    "io.mpv.Mpv" = {
      overrides = {
        filesystems = [
          "xdg-config/mpv"
        ];
      };
    };
  };

  xdg = mkIf (isLinux && config.flatpak.enable && !config.machine.nixos.enable) {
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
