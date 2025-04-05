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

    settings =
      let
        default = {
          criteria = "*";
          mode = "3840x2160";
          position = "0,0";
          scale = 2.0;
        };

        apple_pro_display_xdr = default // {
          criteria = "Apple Computer Inc ProDisplayXDR 0x00001F07";
          mode = "6016x3384";
        };

        lg_27uk650_w = default // {
          criteria = "LG Electronics LG HDR 4K 0x00036193";
          scale = 1.5;
        };

        innocn_pu15_pre = default // {
          criteria = "Beihai Century Joint Innovation Technology Co.,Ltd PU15-PRE FK1UC1R060115";
        };

        cuview_pix3_pro = default // {
          criteria = "DO NOT USE - RTK Pi-X3 Pro demoset-1"; #lol
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
            name = "dual_cuview";
            outputs = [
              (cuview_pix3_pro // { position = "1920,0"; })
              lg_27uk650_w
            ];
          };
        }
        {
          profile = {
            name = "dual_pu15";
            outputs = [
              (innocn_pu15_pre // { position = "1920,0"; })
              lg_27uk650_w
            ];
          };
        }
        {
          profile = {
            name = "dual_xdr_uk650w";
            outputs = [
              (apple_pro_display_xdr // { position = "2560,0"; })
              lg_27uk650_w
            ];
          };
        }
        {
          profile = {
            name = "only_cuview";
            outputs = [
              cuview_pix3_pro
            ];
          };
        }
        {
          profile = {
            name = "only_pu15";
            outputs = [
              innocn_pu15_pre
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
        {
          profile = {
            name = "only_uk650w";
            outputs = [
              lg_27uk650_w
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
      ];
  };
}
