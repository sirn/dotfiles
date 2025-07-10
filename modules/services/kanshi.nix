{ config, lib, pkgs, ... }:

let
  swaymsgBin =
    if config.wayland.windowManager.sway.package != null
    then "${config.wayland.windowManager.sway.package}/bin/swaymsg"
    else "swaymsg";
in
{
  services.kanshi = {
    enable = true;

    systemdTarget = "sway-session.target";

    settings =
      let
        default = {
          status = lib.mkDefault "enable";
          criteria = "*";
          mode = "3840x2160";
          position = "0,0";
          scale = 2.0;
        };

        apple_pro_display_xdr = default // {
          criteria = "Apple Computer Inc ProDisplayXDR 0x00001F07";
          mode = "6016x3384";
        };

        dell_aw3225qf = default // {
          criteria = "Dell Inc. AW3225QF 13T4YZ3";
          scale = 1.5;
        };

        asus_pa148 = {
          criteria = "ASUSTek COMPUTER INC ASUS PA148 N9LMTF061468";
          mode = "1920x1080";
          scale = 1.5;
        };
      in
      [
        {
          profile = {
            name = "only_3225qf";
            outputs = [
              dell_aw3225qf
            ];
          };
        }
        {
          profile = {
            name = "only_xdr";
            outputs = [
              apple_pro_display_xdr
            ];
          };
        }
        {
          profile = {
            name = "only_pa148";
            outputs = [
              asus_pa148
            ];
          };
        }
      ];
  };
}
