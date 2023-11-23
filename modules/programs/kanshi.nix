{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
in
mkIf config.desktop.enable {
  home.packages = with pkgs; [
    kanshi
  ];

  home.file = {
    ".config/kanshi/config" =
      let
        apple_pro_xdr_display = "Apple Computer Inc ProDisplayXDR 0x00001F07";
        lg_27uk650_w = "LG Electronics LG HDR 4K 0x00006393";
        asus_pa148 = "ASUSTek COMPUTER INC ASUS PA148 N9LMTF061468";
      in
      {
        # Note: position must also take scale into account
        # e.g. 3840x2160 at 1.5x scale should use 3840/1.5 = 2560 for x position
        text = ''
          profile main_dual {
            output "${lg_27uk650_w}" mode 3840x2160 position 0,0 scale 1.5
            output "${apple_pro_xdr_display}" mode 6016x3384 position 2560,0 scale 2
            exec swaymsg workspace 1, move workspace to output '"${apple_pro_xdr_display}"'
            exec swaymsg workspace 10, move workspace to output '"${lg_27uk650_w}"'
          }

          profile main_single_lg {
            output "${lg_27uk650_w}" mode 3840x2160 position 0,0 scale 1.5
          }

          profile main_single_xdr {
            output "${apple_pro_xdr_display}" mode 6016x3384 position 0,0 scale 2
          }

          profile main_single_pa148 {
            output "${asus_pa148}" mode 1920x1080 position 0,0 scale 1.5
          }
        '';
      };
  };
}
