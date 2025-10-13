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
        # color: blue, turquoise, green, yellow, orange, red, pink, purple, toolbar
        # icon: briefcase, cart, circle, dollar, fence, fingerprint, gift, vacation, food, fruit, pet, tree, chill
        containers = {
          personal = {
            id = 1;
            color = "green";
            icon = "fingerprint";
          };
          social = {
            id = 2;
            color = "pink";
            icon = "chill";
          };
        };

        containersForce = true;

        extensions = {
          packages = with pkgs.nur.repos.rycee.firefox-addons; [
            dearrow
            kagi-search
            languagetool
            multi-account-containers
            simple-tab-groups
            sponsorblock
            ublock-origin
          ];
        };

        settings = {
          "browser.aboutwelcome.enabled" = false;
          "browser.compactmode.show" = true;
          "browser.contentblocking.category" = "strict";
          "browser.ctrlTab.recentlyUsedOrder" = false;
          "browser.ml.chat.enabled" = false;
          "browser.newtabpage.activity-stream.feeds.section.highlights" = false;
          "browser.newtabpage.activity-stream.feeds.topsites" = false;
          "browser.search.separatePrivateDefault.ui.enabled" = false;
          "browser.sessionstore.warnOnQuit" = true;
          "browser.startup.page" = 3;
          "browser.translations.automaticallyPopup" = false;
          "browser.urlbar.oneOffSearches" = true;
          "browser.urlbar.suggest.searches" = true;
          "browser.urlbar.userMadeSearchSuggestionsChoice" = true;
          "dom.security.https_only_mode" = true;
          "extensions.update.autoUpdateDefault" = false;
          "extensions.autoDisableScopes" = 0;
          "extensions.pocket.enabled" = false;
          "font.cjk_pref_fallback_order" = "ja,zh-cn,zh-hk,zh-tw,ko";
          "identity.fxaccounts.enabled" = false;
          "network.cookie.cookieBehavior" = 1;
          "privacy.trackingprotection.cryptomining.enabled" = true;
          "privacy.trackingprotection.enabled" = true;
          "privacy.trackingprotection.fingerprinting.enabled" = true;
          "signon.prefillForms" = false;
          "signon.rememberSignons" = false;
          "svg.context-properties.content.enabled" = true;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        };
      };
    };
  };
}
