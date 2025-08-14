{ config, lib, pkgs, ... }:

let
  csshacks = pkgs.local.firefox-csshacks;
in
{
  programs.firefox = {
    enable = true;

    # If NixGL is configured (i.e. non-NixOS), wrap with NixGL
    # so OpenGL/Vulkan libraries are available. On Darwin and
    # when we're using Firefox from Flatpak, only configure Firefox.
    package = lib.mkDefault
      (if pkgs.stdenv.isLinux && !config.flatpak.enable
      then config.lib.nixGL.wrap pkgs.firefox
      else null);

    configPath = lib.mkDefault
      (if pkgs.stdenv.isDarwin
      then "Library/Application Support/Firefox"
      else
        if config.flatpak.enable
        then ".var/app/org.mozilla.firefox/.mozilla/firefox"
        else ".mozilla/firefox");

    # By default, this is set to 2, which fails on non-NixOS Firefox
    # https://github.com/nix-community/home-manager/issues/6170
    profileVersion = null;

    profiles = {
      main = {
        extraConfig = ''
          // Behaviors & UI
          user_pref("browser.compactmode.show", true);
          user_pref("browser.ctrlTab.recentlyUsedOrder", false);
          user_pref("browser.download.open_pdf_attachments_inline", true);
          user_pref("browser.newtabpage.activity-stream.feeds.section.highlights", false);
          user_pref("browser.newtabpage.activity-stream.feeds.topsites", false);
          user_pref("browser.search.separatePrivateDefault.ui.enabled", false);
          user_pref("browser.sessionstore.warnOnQuit", true);
          user_pref("browser.startup.page", 3);
          user_pref("browser.urlbar.oneOffSearches", true);
          user_pref("browser.urlbar.suggest.searches", true);
          user_pref("browser.urlbar.userMadeSearchSuggestionsChoice", true);
          user_pref("extensions.pocket.enabled", false);
          user_pref("font.cjk_pref_fallback_order", "ja,zh-cn,zh-hk,zh-tw,ko");
          user_pref("services.sync.addons.ignoreUserEnabledChanges", true);
          user_pref("signon.prefillForms", false);
          user_pref("signon.rememberSignons", false);
          user_pref("svg.context-properties.content.enabled", true);
          user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);

          // Privacy protection
          user_pref("browser.contentblocking.category", "strict");
          user_pref("network.cookie.cookieBehavior", 1);
          user_pref("privacy.trackingprotection.cryptomining.enabled", true);
          user_pref("privacy.trackingprotection.enabled", true);
          user_pref("privacy.trackingprotection.fingerprinting.enabled", true);
          user_pref("dom.security.https_only_mode", true);
        '';
      };
    };
  };
}
