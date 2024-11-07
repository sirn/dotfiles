{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isLinux isDarwin;

  csshacks = pkgs.local.firefox-csshacks;
in
{
  programs.firefox = {
    enable = true;
    package = if isLinux then pkgs.firefox else null;

    profiles = {
      main = {
        extraConfig = ''
          // Behaviors & UI
          user_pref("browser.ctrlTab.recentlyUsedOrder", false);
          user_pref("browser.newtabpage.activity-stream.feeds.section.highlights", false);
          user_pref("browser.newtabpage.activity-stream.feeds.topsites", false);
          user_pref("browser.search.separatePrivateDefault.ui.enabled", true);
          user_pref("browser.sessionstore.warnOnQuit", true);
          user_pref("browser.startup.page", 3);
          user_pref("browser.compactmode.show", true);
          user_pref("browser.urlbar.oneOffSearches", true);
          user_pref("browser.urlbar.suggest.searches", true);
          user_pref("browser.urlbar.userMadeSearchSuggestionsChoice", true);
          user_pref("extensions.pocket.enabled", false);
          user_pref("services.sync.addons.ignoreUserEnabledChanges", true);
          user_pref("signon.prefillForms", false);
          user_pref("signon.rememberSignons", false);
          user_pref("font.cjk_pref_fallback_order", "ja,zh-cn,zh-hk,zh-tw,ko");
          user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
          user_pref("svg.context-properties.content.enabled", true);

          // Rendering
          user_pref("gfx.color_management.enablev4", true);
          user_pref("gfx.color_management.mode", 1);
          user_pref("gfx.color_management.rendering_intent", 0);
          user_pref("gfx.compositor.glcontext.opaque", true);

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
