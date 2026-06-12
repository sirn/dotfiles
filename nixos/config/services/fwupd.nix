{
  services.fwupd = {
    enable = true;
  };

  # Grant fwupd-refresh system user access to metadata refresh without polkit
  # auth (headless service user has no seat).
  # TODO: Remove once nixpkgs includes PR #526476.
  security.polkit.extraConfig = ''
    polkit.addRule(function(action, subject) {
      if ((action.id == "org.freedesktop.fwupd.get-remotes" ||
           action.id == "org.freedesktop.fwupd.refresh-remote") &&
          subject.user == "fwupd-refresh") {
        return polkit.Result.YES;
      }
    });
  '';
}
