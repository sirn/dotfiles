{ config, pkgs, ... }:

# Note: tracking unstable to fix issue with pylsp-mypy broken on Darwin.
# Might be safe to switch this back to stable after > 23.05.
#
# See also: https://github.com/NixOS/nixpkgs/issues/235603

let
  inherit (pkgs.stdenv) isDarwin;

  dnspython_2_0_0 = pkgs.unstable.python3Packages.dnspython.overridePythonAttrs (orig: rec {
    version = "2.0.0";
    src = pkgs.fetchFromGitHub {
      inherit version;
      owner = "rthalley";
      repo = "dnspython";
      rev = "v${version}";
      sha256 = "sha256-jxsRRdOm0w44E5sgQ/Rdwy4Unel31IvvEkJOEjOAQRI=";
    };

    propagatedBuildInputs = [
      pkgs.unstable.python3Packages.trio
      pkgs.unstable.iana-etc
    ];
  });

  ipwhois = pkgs.unstable.python3Packages.buildPythonPackage rec {
    pname = "ipwhois";
    version = "1.2.0";
    src = pkgs.unstable.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "sha256-gx4/7zuAQSAaBI6UVuO4D0XGyLcXTzTtIR9mtZbIS74=";
    };
    propagatedBuildInputs = [
      dnspython_2_0_0
    ];
  };

  pythonEnv = pkgs.unstable.python311.withPackages (p: with p; [
    black
    flake8
    ipwhois
    pip-tools
    poetry-core
    pyls-flake8
    pyls-isort
    pylsp-mypy
    python-lsp-black
    python-lsp-server
  ]);
in
{
  home.packages = [
    pythonEnv
  ];
}
