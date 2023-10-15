{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    kanshi
  ];

  home.file = {
    ".config/kanshi/config" =
      let
        apple_pro_xdr_display = "Apple Computer Inc ProDisplayXDR 0x00001F07";
        lg_27uk650_w = "LG Electronics LG HDR 4K 0x00006393";
      in
      {
        text = ''
          profile main_dual {
            output "${apple_pro_xdr_display}" position 2194,0 scale 2
            output "${lg_27uk650_w}" position 0,0 scale 1.5
            exec swaymsg workspace 1, move workspace to output '"${apple_pro_xdr_display}"'
            exec swaymsg workspace 10, move workspace to output '"${lg_27uk650_w}"'
          }

          profile main_single_lg {
            output "${lg_27uk650_w}" position 0,0 scale 1.5
            exec swaymsg workspace 1, move workspace to output '"${lg_27uk650_w}"'
          }

          profile main_single_xdr {
            output "${apple_pro_xdr_display}" position 0,0 scale 2
            exec swaymsg workspace 1, move workspace to output '"${apple_pro_xdr_display}"'
          }
        '';
      };
  };
}
