{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    # net
    weechat
    weechatScripts.buffer_autoset
    weechatScripts.colorize_nicks
    weechatScripts.weechat-autosort
    weechatScripts.weechat-go
    weechatScripts.zncplayback

    # graphics
    graphviz-nox
    imagemagick
  ];
}
