{ stdenv, fetchFromGitHub, beamPackages, git, cacert }:

with beamPackages;

# TODO: prevent fetch during build.
rebar3Relx rec {
  name = "erlang-ls-${version}";
  version = "0.9.0";
  releaseType = "escript";

  buildInputs = [ git cacert ];

  src = fetchFromGitHub {
    owner = "erlang-ls";
    repo = "erlang_ls";
    rev = "${version}";
    sha256 = "1caxnmrx2i4mp96hv9xa7lc5ildriz5if6b4g2by7g2pmykmy417";
  };

  buildPhase=''
    HOME=. make all
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp _build/default/bin/erlang_ls $out/bin/erlang_ls
  '';

  impureEnvVars = stdenv.lib.fetchers.proxyImpureEnvVars;
  meta = {
    homepage = "https://github.com/erlang-ls/erlang_ls";
    description = "The Erlang Language Server";
    license = stdenv.lib.licenses.asl20;
  };
}
