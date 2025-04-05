{ config, lib, pkgs, ... }:

let
  # Copied from home-manager/modules/programs/mpv.nix
  renderOption = with lib; option:
    rec {
      int = toString option;
      float = int;
      bool = lib.hm.booleans.yesNo option;
      string = option;
    }.${builtins.typeOf option};

  renderOptionValue = with lib; value:
    let
      rendered = renderOption value;
      length = toString (builtins.stringLength rendered);
    in
    "%${length}%${rendered}";

  renderOptions = with lib; generators.toKeyValue {
    mkKeyValue = generators.mkKeyValueDefault { mkValueString = renderOptionValue; } "=";
    listsAsDuplicateKeys = true;
  };
in
{
  programs.mpv = {
    # On a non-NixOS, enabling mpv via Home-Manager can cause mpv
    # to fallback to vo=xv, which is bad.
    enable = config.machine.isNixOS;

    profiles = lib.mkIf pkgs.stdenv.isLinux {
      hdr = {
        profile-cond = "get(\"video-params/primaries\") == \"bt.2020\"";
        target-colorspace-hint = true;
        target-contrast = "inf";
      };
    };

    config = lib.mkMerge [
      {
        hwdec = "auto";
      }
      (lib.mkIf pkgs.stdenv.isLinux {
        vo = "gpu-next";
        gpu-api = "vulkan";
      })
    ];
  };

  # On a non-NixOS, we only configure MPV without installing
  # the package (mpv should be installed via system's package
  # manager).
  xdg = lib.mkIf (!config.machine.isNixOS) {
    configFile = {
      "mpv/mpv.conf" = {
        text = with lib;
          let
            cfg = config.programs.mpv;
          in
          ''
            ${lib.optionalString
              (cfg.defaultProfiles != [ ])
              (renderOptions { profile = lib.concatStringsSep "," cfg.defaultProfiles; })}
            ${lib.optionalString (cfg.config != { }) (renderOptions cfg.config)}
            ${lib.optionalString (cfg.profiles != { }) (renderOptions cfg.profiles)}
          '';
      };
    };
  };
}
