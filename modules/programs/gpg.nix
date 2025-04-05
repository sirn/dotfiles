{ lib, ... }:

{
  programs.gpg = {
    enable = lib.mkDefault true;

    settings = {
      default-key = "0x65513F82ACB5F854";
    };
  };
}
