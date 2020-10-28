{ lib, stdenv, python3, fetchFromGitHub }:

let
  python3_pyls = python3.override {
    packageOverrides = self: super: {
      python-jsonrpc-server = super.buildPythonPackage rec {
        pname = "python-jsonrpc-server";
        version = "0.4.0";

        propagatedBuildInputs = with self; [
          future
          ujson
        ];

        postPatch = ''
          sed -i 's/version=versioneer.get_version(),/version="${version}",/g' setup.py
        '';

        doCheck = false;

        src = fetchFromGitHub {
          owner = "palantir";
          repo = "python-jsonrpc-server";
          rev = version;
          sha256 = "0pcf50qvcxqnz3db58whqd8z89cdph19pfs1whgfm0zmwbwk0lw6";
        };

        meta = with stdenv.lib; {
          homepage = "https://github.com/palantir/python-jsonrpc-server";
          description = "A Python 2 and 3 asynchronous JSON RPC server";
          license = licenses.mit;
          maintainers = [ maintainers.mic92 ];
        };
      };

      python-language-server = super.buildPythonPackage rec {
        pname = "python-language-server";
        version = "0.35.1";

        propagatedBuildInputs = with self; [
          setuptools
          jedi
          pluggy
          future
          python-jsonrpc-server
          flake8
          ujson
        ];

        doCheck = false;

        src = super.fetchPypi {
          inherit pname version;
          sha256 = "08djjb6n1nln4gykxxbswbwnn2hr9qy6pvcq5sr0x3p958xrl33f";
        };

        meta = with stdenv.lib; {
          homepage = "https://github.com/palantir/python-language-server";
          description = "An implementation of the Language Server Protocol for Python";
          license = licenses.mit;
          maintainers = [ maintainers.mic92 ];
        };
      };

      ujson = super.buildPythonPackage rec {
        pname = "ujson";
        version = "3.1.0";
        disabled = super.isPyPy || (!super.isPy3k);

        src = super.fetchPypi {
          inherit pname version;
          sha256 = "1xqcyp97a1v2yk0lvd19zas5dlfzfnf1h0krh60zxmjy4zga3g80";
        };

        nativeBuildInputs = with self; [ setuptools_scm ];

        meta = with stdenv.lib; {
          homepage = "https://pypi.python.org/pypi/ujson";
          description = "Ultra fast JSON encoder and decoder for Python";
          license = licenses.bsd3;
        };
      };
    };
  };
in

python3_pyls.pkgs
