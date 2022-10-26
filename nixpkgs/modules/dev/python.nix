{ config, pkgs, ... }:

# Note: tracking unstable to fix issue with pyOpenSSL broken on Darwin
# and using python3Packages.buildPythonPackage will prevent installation
# of the entire derivation.
#
# Might be safe to switch this back to stable after > 22.05.
#
# See also: https://github.com/pyca/pyopenssl/issues/873

let
  inherit (pkgs.stdenv) isDarwin;

  dnspython_2_0_0 = pkgs.unstable.python3Packages.dnspython.overridePythonAttrs (orig: rec {
    version = "2.0.0";
    src = pkgs.unstable.fetchFromGitHub {
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

  pythonEnv = pkgs.unstable.python310.withPackages (p: with p; [
    black
    flake8
    ipwhois
    pip-tools
    poetry
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
