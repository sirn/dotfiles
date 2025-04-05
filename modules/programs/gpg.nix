{ lib, ... }:

{
  programs.gpg = {
    enable = lib.mkDefault true;

    settings = {
      default-key = "0x48AB9BABBE427D60";
    };
  };
}
