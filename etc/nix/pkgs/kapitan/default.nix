{ lib, stdenv, python3, fetchFromGitHub }:

let
  # Kapitan requires very specific version of a package.
  python3_kapitan = python3.override {
    packageOverrides = self: super: {
      addict = super.buildPythonPackage rec {
        name = "${python3.libPrefix}-${pname}-${version}";
        pname = "addict";
        version = "2.2.1";

        doCheck = false;
        src = super.fetchPypi {
          inherit pname version;
          sha256 = "1c5qq8ri8sjlm835sk4rzk479vx8rd7l90hsjgj50bq3avq9697j";
        };
      };
      docker = super.docker.overrideDerivation (oldAttrs: rec {
        name = "${python3.libPrefix}-${pname}-${version}";
        pname = oldAttrs.pname;
        version = "4.2.1";

        src = super.fetchPypi {
          inherit pname version;
          sha256 = "1wk416f0rsybac3c7wbzsng0zswvmlihskgfjqp8gamziz9j02iq";
        };
      });
      google_api_python_client = super.google_api_python_client.overrideDerivation (oldAttrs: rec {
        name = "${python3.libPrefix}-${pname}-${version}";
        pname = oldAttrs.pname;
        version = "1.7.11";

        src = super.fetchPypi {
          inherit pname version;
          sha256 = "137vwb9544vjxkwnbr98x0f4p6ri5i678wxxxgbsx4kdyrs83a58";
        };
      });
      GitPython = super.GitPython.overrideDerivation (oldAttrs: rec {
        name = "${python3.libPrefix}-${pname}-${version}";
        pname = oldAttrs.pname;
        version = "3.1.3";

        src = super.fetchPypi {
          inherit pname version;
          sha256 = "06nsz31328zd45wgqn7m7szczw49kywbxx5l913ddbixhx6sy1z1";
        };
      });
      hvac = super.buildPythonPackage rec {
        name = "${python3.libPrefix}-${pname}-${version}";
        pname = "hvac";
        version = "0.10.4";

        pythonPath = with self; [ requests six ];
        propagatedBuildInputs = pythonPath;

        doCheck = false;
        src = super.fetchPypi {
          inherit pname version;
          sha256 = "0yhywm8f86pc4f7ivvbwicwhzf0khjqp9jj77pqy6nha6znvpvnh";
        };
      };
      jsonschema = super.jsonschema.overrideDerivation (oldAttrs: rec {
        name = "${python3.libPrefix}-${pname}-${version}";
        pname = oldAttrs.pname;
        version = "3.2.0";

        src = super.fetchPypi {
          inherit pname version;
          sha256 = "0ykr61yiiizgvm3bzipa3l73rvj49wmrybbfwhvpgk3pscl5pa68";
        };
      });
      jsonnet = super.jsonnet.overrideDerivation (oldAttrs: rec {
        name = "${python3.libPrefix}-${pname}-${version}";
        pname = "jsonnet";
        version = "0.16.0";

        doCheck = false;
        src = fetchFromGitHub {
          rev = "v${version}";
          owner = "google";
          repo = "jsonnet";
          sha256 = "0wxhc0ghjk88zrrn9a18h979ipkia2rx12489515gzhlplfx6724";
        };
      });
      python-gnupg = super.python-gnupg.overrideDerivation (oldAttrs: rec {
        name = "${python3.libPrefix}-${pname}-${version}";
        pname = oldAttrs.pname;
        version = "0.4.6";

        src = super.fetchPypi {
          inherit pname version;
          sha256 = "16h1aa3fbjcv131zrf51xk174wmqwwwxzfc54cn6a56l7d5qi81s";
        };
      });
      requests = super.requests.overrideDerivation (oldAttrs: rec {
        name = "${python3.libPrefix}-${pname}-${version}";
        pname = oldAttrs.pname;
        version = "2.23.0";

        src = super.fetchPypi {
          inherit pname version;
          sha256 = "1rhpg0jb08v0gd7f19jjiwlcdnxpmqi1fhvw7r4s9avddi4kvx5k";
        };
      });
      rfc3987 = super.rfc3987.overrideDerivation (oldAttrs: rec {
        name = "${python3.libPrefix}-${pname}-${version}";
        pname = oldAttrs.pname;
        version = "1.3.8";

        src = super.fetchPypi {
          inherit pname version;
          sha256 = "0cx7bhyjaylwnf9armxrkb3r0s37vf0vqf3bhbll9mb0lmbx5i6k";
        };
      });
      toml = super.toml.overrideDerivation (oldAttrs: rec {
        name = "${python3.libPrefix}-${pname}-${version}";
        pname = oldAttrs.pname;
        version = "0.10.1";

        src = super.fetchPypi {
          inherit pname version;
          sha256 = "03wbqm5cn685cwx2664hjdpz370njl7lf0yal8s0dkp5w4mn2swj";
        };
      });
    };
  };
in
with python3_kapitan.pkgs;

buildPythonPackage rec {
  pname = "kapitan";
  version = "0.29.3";

  src = fetchPypi {
    inherit pname version;
    sha256 = "1cjda0jh2wrypf2q4l8v4v6klkb1zz31hrz3lnkvspp9cphd6aqi";
  };

  pythonPath = [
    GitPython
    addict
    boto3
    cffi
    cryptography
    docker
    google_api_python_client
    hvac
    jinja2
    jsonnet
    jsonschema
    pyparsing
    python-gnupg
    requests
    rfc3987
    toml
    yamllint
  ];

  propagatedBuildInputs = pythonPath;

  doCheck = false;
  meta = {
    homepage = "https://github.com/deepmind/kapitan";
    description = "Generic templated configuration management";
    license = stdenv.lib.licenses.asl20;
  };
}
