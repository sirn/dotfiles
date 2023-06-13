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

  dnspython_2_0_0 = pkgs.python3Packages.dnspython.overridePythonAttrs (orig: rec {
    version = "2.0.0";
    src = pkgs.fetchFromGitHub {
      inherit version;
      owner = "rthalley";
      repo = "dnspython";
      rev = "v${version}";
      sha256 = "sha256-jxsRRdOm0w44E5sgQ/Rdwy4Unel31IvvEkJOEjOAQRI=";
    };

    propagatedBuildInputs = [
      pkgs.python3Packages.trio
      pkgs.iana-etc
    ];
  });

  ipwhois = pkgs.python3Packages.buildPythonPackage rec {
    pname = "ipwhois";
    version = "1.2.0";
    src = pkgs.python3Packages.fetchPypi {
      inherit pname version;
      sha256 = "sha256-gx4/7zuAQSAaBI6UVuO4D0XGyLcXTzTtIR9mtZbIS74=";
    };
    propagatedBuildInputs = [
      dnspython_2_0_0
    ];
  };

  pythonEnv = pkgs.python310.withPackages (p: with p; [
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
