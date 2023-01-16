{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    weechat
    weechatScripts.buffer_autoset
    weechatScripts.colorize_nicks
    weechatScripts.weechat-autosort
    weechatScripts.weechat-go
    weechatScripts.zncplayback
  ];
}
