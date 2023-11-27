{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    (curl.override {
      brotliSupport = true;
      http3Support = true;
      idnSupport = true;
      openssl = pkgs.quictls;
      zstdSupport = true;
    })
  ];
}
