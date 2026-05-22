{
  config,
  lib,
  pkgs,
  ...
}:

let
  firefoxProfiles = config.programs.firefox.profiles;
  firefoxExec =
    if pkgs.stdenv.isDarwin then
      "/Applications/Firefox.app/Contents/MacOS/firefox"
    else if config.flatpak.enable then
      "flatpak run org.mozilla.firefox"
    else
      lib.getExe config.programs.firefox.finalPackage;

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

        cat > "$appDir/Contents/MacOS/${execName}" << 'EOF'
        #!/bin/bash
        exec open -n -a /Applications/Firefox.app --args -P "${name}" -no-remote
        EOF
        chmod +x "$appDir/Contents/MacOS/${execName}"

        if [ -f "/Applications/Firefox.app/Contents/Resources/firefox.icns" ]; then
          cp "/Applications/Firefox.app/Contents/Resources/firefox.icns" "$appDir/Contents/Resources/"
        fi

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
          "signon.rememberSignons" = false;

          # WebRTC IP leak protection
          "media.peerconnection.ice.default_address_only" = true;

          # Extensions
          "extensions.pocket.enabled" = false;

          # AI features
          "browser.ml.enable" = false;
          "browser.ml.chat.enabled" = false;
          "browser.ml.chat.page" = false;
          "browser.ml.chat.shortcuts" = false;
          "browser.ml.chat.sidebar" = false;

          # Fonts
          "font.cjk_pref_fallback_order" = "ja,zh-cn,zh-hk,zh-tw,ko";

          # Telemetry & data collection
          "app.shield.optoutstudies.enabled" = false;
          "browser.crashReports.unsubmittedCheck.enabled" = false;
          "browser.newtabpage.activity-stream.showSponsored" = false;
          "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
          "browser.newtabpage.activity-stream.feeds.telemetry" = false;
          "browser.newtabpage.activity-stream.telemetry" = false;
          "browser.urlbar.quicksuggest.enabled" = false;
          "datareporting.healthreport.uploadEnabled" = false;
          "datareporting.policy.dataSubmissionEnabled" = false;
          "toolkit.telemetry.server" = "data:,";
          "toolkit.telemetry.unified" = false;
        };
      };
    };
  };

  # Linux: XDG desktop entries for non-default Firefox profiles
  xdg.desktopEntries = lib.mkIf (config.programs.firefox.enable && pkgs.stdenv.isLinux) (
    lib.mapAttrs' (name: profile: {
      name = "firefox-${name}";
      value = {
        name = "Firefox (${name})";
        genericName = "Web Browser";
        exec = "${firefoxExec} -P ${name} %U";
        icon = "org.mozilla.firefox";
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
    }) (lib.filterAttrs (name: _: name != "main") firefoxProfiles)
  );

  # macOS: Application bundles for non-default Firefox profiles
  home.packages = lib.mkIf (config.programs.firefox.enable && pkgs.stdenv.isDarwin) (
    map (name: mkFirefoxProfileApp name) (builtins.filter (name: name != "main") (builtins.attrNames firefoxProfiles))
  );
}
