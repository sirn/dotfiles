{ config, pkgs, ... }:

let
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
    poetry
    pyls-flake8
    pyls-isort
    pylsp-mypy
    python-lsp-black
    python-lsp-server
  ]);
in
{
  home.packages = with pkgs.python310Packages; [
    pythonEnv
  ];
}
