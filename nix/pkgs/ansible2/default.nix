{ stdenv, fetchurl, ansible2 }:

ansible2.overrideDerivation (oldAttrs: rec {
  version = "2.1.0.0";
  name = "ansible-${version}";

  src = fetchurl {
    url = "mirror://pypi/a/ansible/${name}.tar.gz";
    sha256 = "1bfc2xiplpad6f2nwi48y0kps7xqnsll85dlz63cy8k5bysl6d20";
  };
})