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

    # multimedia
    ffmpeg
    yt-dlp

    # graphics
    graphviz-nox
    imagemagick
  ];
}
