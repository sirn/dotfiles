{ pkgs, ... }: {
  projectRootFile = "flake.nix";

  # Enable formatters
  programs.nixfmt = {
    enable = true;
    package = pkgs.nixfmt;
    strict = true;
  };

  programs.prettier = {
    enable = true;
    settings.proseWrap = "never";
  };

  programs.shfmt.enable = true;

  # Global settings
  settings = {
    excludes = [
      "*.sops.*"
      "flake.lock"
      "secrets/**"
    ];
  };
}
