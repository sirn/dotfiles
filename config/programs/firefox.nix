{
  config,
  lib,
  pkgs,
  ...
}:

let
  csshacks = pkgs.local.firefox-csshacks;

  firefoxProfiles = config.programs.firefox.profiles;
  firefoxExec =
    if pkgs.stdenv.isDarwin then
      "/Applications/Firefox.app/Contents/MacOS/firefox"
    else if config.flatpak.enable then
      "flatpak run org.mozilla.firefox"
    else
      lib.getExe config.programs.firefox.finalPackage;

  # Create a macOS app bundle for a Firefox profile
  mkFirefoxProfileApp =
    name:
    let
      bundleId = "org.nix-community.home.firefox.${name}";
      appName = "Firefox (${name})";
      execName = "firefox-${name}";
    in
    pkgs.runCommand "firefox-${name}-app"
      {
        meta = {
          platforms = lib.platforms.darwin;
        };
      }
      ''
        appDir="$out/Applications/${appName}.app"
        mkdir -p "$appDir/Contents/MacOS"
        mkdir -p "$appDir/Contents/Resources"

        # Create the launcher script
        cat > "$appDir/Contents/MacOS/${execName}" << 'EOF'
        #!/bin/bash
        exec open -n -a /Applications/Firefox.app --args -P "${name}" -no-remote
        EOF
        chmod +x "$appDir/Contents/MacOS/${execName}"

        # Try to copy Firefox icon from system Firefox
        if [ -f "/Applications/Firefox.app/Contents/Resources/firefox.icns" ]; then
          cp "/Applications/Firefox.app/Contents/Resources/firefox.icns" "$appDir/Contents/Resources/"
        fi

        # Create Info.plist
        cat > "$appDir/Contents/Info.plist" << 'PLISTEOF'
        <?xml version="1.0" encoding="UTF-8"?>
        <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
        <plist version="1.0">
        <dict>
          <key>CFBundleInfoDictionaryVersion</key>
          <string>6.0</string>
          <key>CFBundleDevelopmentRegion</key>
          <string>en</string>
          <key>CFBundlePackageType</key>
          <string>APPL</string>
          <key>CFBundleIdentifier</key>
          <string>${bundleId}</string>
          <key>CFBundleExecutable</key>
          <string>${execName}</string>
          <key>CFBundleName</key>
          <string>${appName}</string>
          <key>CFBundleDisplayName</key>
          <string>${appName}</string>
          <key>CFBundleVersion</key>
          <string>1.0</string>
          <key>CFBundleShortVersionString</key>
          <string>1.0</string>
          <key>CFBundleIconFile</key>
          <string>firefox</string>
          <key>LSUIElement</key>
          <false/>
          <key>NSHighResolutionCapable</key>
          <true/>
          <key>NSSupportsAutomaticGraphicsSwitching</key>
          <true/>
        </dict>
        </plist>
        PLISTEOF
      '';
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
            auto-tab-discard
            dearrow
            history-cleaner
            kagi-search
            languagetool
            sponsorblock
            ublock-origin
          ];
        };

        settings = {
          # Disable fractional-scale to force 2x rendering then downscale.
          # This results in sharper image quality than native fractional scaling.
          "widget.wayland.fractional-scale.enabled" = false;

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
          "svg.context-properties.content.enabled" = true;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;

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

  # macOS: Raycast script commands
  home.file = lib.mkIf (config.programs.firefox.enable && pkgs.stdenv.isDarwin) (
    lib.mapAttrs' (name: profile: {
      name = ".local/libexec/raycast/firefox-${name}.sh";
      value = {
        executable = true;
        text = ''
          #!/bin/bash

          # @raycast.schemaVersion 1
          # @raycast.title Firefox - ${name}
          # @raycast.mode silent
          # @raycast.icon 🦊
          # @raycast.packageName Firefox Profiles

          ${firefoxExec} -P "${name}" -no-remote &
          echo "Launched Firefox with profile ${name}"
        '';
      };
    }) firefoxProfiles
  );

  # Linux: XDG desktop entries
  xdg.desktopEntries = lib.mkIf (config.programs.firefox.enable && pkgs.stdenv.isLinux) (
    lib.mapAttrs' (name: profile: {
      name = "firefox-${name}";
      value = {
        name = "Firefox (${name})";
        genericName = "Web Browser";
        exec = "${firefoxExec} -P ${name} %U";
        icon = "firefox";
        terminal = false;
        categories = [
          "Network"
          "WebBrowser"
        ];
        mimeType = [
          "text/html"
          "text/xml"
          "application/xhtml+xml"
        ];
        startupNotify = true;
      };
    }) firefoxProfiles
  );

  # macOS: Application bundles for each profile
  # These are copied to ~/Applications/Home Manager Apps/ by Home Manager's
  # copyApps feature, making them available in Spotlight, Dock, etc.
  home.packages = lib.mkIf (config.programs.firefox.enable && pkgs.stdenv.isDarwin) (
    map (name: mkFirefoxProfileApp name) (builtins.attrNames firefoxProfiles)
  );
}
