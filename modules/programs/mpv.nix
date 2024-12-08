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
    # On a non-NixOS, enabling mpv via Home-Manager can cause mpv
    # to fallback to vo=xv, which is bad.
    enable = config.machine.isNixOS;

    profiles = mkIf isLinux {
      hdr = {
        profile-cond = "get(\"video-params/primaries\") == \"bt.2020\"";
        target-colorspace-hint = true;
        target-contrast = "inf";
      };
    };

    config = {
      hwdec = "auto";
    } // mkIf isLinux {
      vo = "gpu-next";
      gpu-api = "vulkan";
    };
  };

  # On a non-NixOS, we only configure MPV without installing
  # the package (mpv should be installed via system's package
  # manager).
  xdg = mkIf (!config.machine.isNixOS) {
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
