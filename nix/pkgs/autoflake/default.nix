{ stdenv, fetchurl, python35Packages }:

with python35Packages;

buildPythonPackage rec {
  name = "autoflake-${version}";
  version = "0.6.6";

  src = fetchurl {
    url = "mirror://pypi/a/autoflake/${name}.tar.gz";
    sha256 = "03qfycfm9bvfvbkfh6962kd0ia0gx6ffnizajsb4nqgqfimqiwv7";
  };

  propagatedBuildInputs = [ pyflakes ];

  meta = {
    homepage = "https://github.com/myint/autoflake";
    description = "Removes unused imports and unused variables as reported by pyflakes";
    license = stdenv.lib.licenses.mit;
  };
}