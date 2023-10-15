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
            output "${apple_pro_xdr_display}" mode 6016x3384 position 2194,0 scale 2
            output "${lg_27uk650_w}" mode 3840x2160 position 0,0 scale 1.75
            exec swaymsg workspace 1, move workspace to output '"${apple_pro_xdr_display}"'
          }

          profile main_single {
            output "${lg_27uk650_w}" mode 3840x2160 position 0,0 scale 1.75
            exec swaymsg workspace 1, move workspace to output '"${lg_27uk650_w}"'
          }
        '';
      };
  };
}
