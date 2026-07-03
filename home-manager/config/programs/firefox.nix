{
  config,
  lib,
  pkgs,
  ...
}:

let
  firefoxProfiles = config.programs.firefox.profiles;
in
{
  programs.firefox = {
    enable = true;

    # If NixGL is configured (i.e. non-NixOS), wrap with NixGL so OpenGL/Vulkan libraries are available.
    # On macOS, user should install Firefox by themselves.
    package = lib.mkDefault (if pkgs.stdenv.isLinux then config.lib.nixGL.wrap pkgs.firefox else null);

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
          "browser.startup.page" = 3;

          # WebRTC IP leak protection
          "media.peerconnection.ice.default_address_only" = true;

          # Extensions
          "extensions.pocket.enabled" = false;

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
  xdg.desktopEntries =
    let
      firefoxExec = lib.getExe config.programs.firefox.finalPackage;
    in
    lib.mkIf (config.programs.firefox.enable && pkgs.stdenv.isLinux) (
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
  home.packages =
    let
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
    lib.mkIf (config.programs.firefox.enable && pkgs.stdenv.isDarwin) (
      map (name: mkFirefoxProfileApp name) (
        builtins.filter (name: name != "main") (builtins.attrNames firefoxProfiles)
      )
    );
}
