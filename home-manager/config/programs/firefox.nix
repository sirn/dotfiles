{
  config,
  lib,
  pkgs,
  ...
}:

let
  firefoxExec =
    if pkgs.stdenv.isDarwin then
      "/Applications/Firefox.app/Contents/MacOS/firefox"
    else if config.flatpak.enable then
      "flatpak run org.mozilla.firefox"
    else
      lib.getExe config.programs.firefox.finalPackage;
in
{
  programs.firefox = {
    enable = true;

    # If NixGL is configured (i.e. non-NixOS), wrap with NixGL
    # so OpenGL/Vulkan libraries are available. On Darwin and
    # when we're using Firefox from Flatpak, only configure Firefox.
    package = lib.mkDefault (
      if pkgs.stdenv.isLinux && !config.flatpak.enable then config.lib.nixGL.wrap pkgs.firefox else null
    );

    configPath = lib.mkDefault (
      if pkgs.stdenv.isDarwin then
        "Library/Application Support/Firefox"
      else if config.flatpak.enable then
        ".var/app/org.mozilla.firefox/.mozilla/firefox"
      else
        ".mozilla/firefox"
    );

    # By default, this is set to 2, which fails on non-NixOS Firefox
    # https://github.com/nix-community/home-manager/issues/6170
    profileVersion = null;

    profiles = {
      main = {
        extensions = {
          packages = with pkgs.nur.repos.rycee.firefox-addons; [
            dearrow
            kagi-search
            languagetool
            sponsorblock
            ublock-origin
          ];
        };

        settings = {
          # Privacy & security
          "browser.contentblocking.category" = "strict";
          "dom.security.https_only_mode" = true;
          "network.cookie.cookieBehavior" = 5;
          "privacy.trackingprotection.cryptomining.enabled" = true;
          "privacy.trackingprotection.enabled" = true;
          "privacy.trackingprotection.fingerprinting.enabled" = true;

          # Session management
          "browser.sessionstore.warnOnQuit" = true;
          "browser.startup.page" = 3;

          # Password manager
          "signon.prefillForms" = false;
          "signon.rememberSignons" = false;

          # Search & URL bar
          "browser.search.separatePrivateDefault.ui.enabled" = false;
          "browser.urlbar.oneOffSearches" = true;
          "browser.urlbar.suggest.searches" = true;
          "browser.urlbar.userMadeSearchSuggestionsChoice" = true;

          # New tab page
          "browser.newtabpage.activity-stream.feeds.section.highlights" = false;
          "browser.newtabpage.activity-stream.feeds.topsites" = false;

          # UI & UX
          "browser.aboutwelcome.enabled" = false;
          "browser.bookmarks.addedImportButton" = true;
          "browser.compactmode.show" = true;
          "browser.ctrlTab.recentlyUsedOrder" = false;
          "browser.profiles.enabled" = false;
          "sidebar.revamp" = true;

          # Extensions
          "extensions.autoDisableScopes" = 0;
          "extensions.pocket.enabled" = false;
          "extensions.update.autoUpdateDefault" = false;

          # AI features
          "browser.ml.chat.enabled" = false;
          "browser.ml.chat.menu" = false;

          # Translations
          "browser.translations.automaticallyPopup" = false;

          # Fonts
          "font.cjk_pref_fallback_order" = "ja,zh-cn,zh-hk,zh-tw,ko";

          # Firefox Sync
          "identity.fxaccounts.enabled" = false;

          # Telemetry & data collection
          "app.normandy.api_url" = "";
          "app.normandy.enabled" = false;
          "app.shield.optoutstudies.enabled" = false;
          "browser.crashReports.unsubmittedCheck.autoSubmit2" = false;
          "browser.crashReports.unsubmittedCheck.enabled" = false;
          "datareporting.healthreport.uploadEnabled" = false;
          "datareporting.policy.dataSubmissionEnabled" = false;
          "toolkit.telemetry.archive.enabled" = false;
          "toolkit.telemetry.bhrPing.enabled" = false;
          "toolkit.telemetry.enabled" = false;
          "toolkit.telemetry.firstShutdownPing.enabled" = false;
          "toolkit.telemetry.newProfilePing.enabled" = false;
          "toolkit.telemetry.server" = "data:,";
          "toolkit.telemetry.shutdownPingSender.enabled" = false;
          "toolkit.telemetry.unified" = false;
          "toolkit.telemetry.updatePing.enabled" = false;

          # WebRTC IP leak protection
          "media.peerconnection.ice.default_address_only" = true;
          "media.peerconnection.ice.proxy_only_if_behind_proxy" = true;

          # Referrer policy
          "network.http.referer.XOriginTrimmingPolicy" = 2;

          # Pocket & sponsored content
          "browser.newtabpage.activity-stream.default.sites" = "";
          "browser.newtabpage.activity-stream.feeds.telemetry" = false;
          "browser.newtabpage.activity-stream.showSponsored" = false;
          "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
          "browser.newtabpage.activity-stream.telemetry" = false;
          "browser.urlbar.pocket.featureGate" = false;

          # Location & geolocation
          "browser.fixup.alternate.enabled" = false;
          "geo.provider.network.url" = "";
          "geo.provider.use_corelocation" = false;

          # Cleanup on shutdown
          "privacy.sanitize.sanitizeOnShutdown" = true;
          "privacy.clearOnShutdown.cache" = true;
          "privacy.clearOnShutdown.cookies" = false;
          "privacy.clearOnShutdown.downloads" = true;
          "privacy.clearOnShutdown.formdata" = true;
          "privacy.clearOnShutdown.history" = false;
          "privacy.clearOnShutdown.sessions" = false;

          # Additional AI features
          "browser.ml.enable" = false;
          "browser.ml.linkPreview.enabled" = false;
        };
      };
    };
  };
}
