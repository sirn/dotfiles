{ pkgs, ... }:

{
  xdg.configFile."autostart/dropbox.desktop".text = ''
    [Desktop Entry]
    Name=Dropbox
    Exec=${pkgs.writeScriptBin "start-dropbox" ''
      #!${pkgs.bash}/bin/bash
      if command -v flatpak >/dev/null && flatpak info com.dropbox.Client >/dev/null 2>&1; then
        flatpak run com.dropbox.Client
      fi
    ''}/bin/start-dropbox
    Terminal=false
    Type=Application
  '';
}
