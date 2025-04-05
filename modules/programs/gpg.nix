{ lib, ... }:

{
  programs.gpg = {
    enable = lib.mkDefault true;

    settings = {
      default-key = "0x48AB9BABBE427D60";
      local-user = [
        "0x65513F82ACB5F854!"
        "0x8C3D18344F9EB4A1!"
      ];
    };
  };
}
