{ stdenv, fetchFromGitHub, beamPackages, git, cacert }:

with beamPackages;

# TODO: prevent fetch during build.
rebar3Relx rec {
  name = "erlang-ls-${version}";
  version = "0.4.1";
  releaseType = "escript";

  buildInputs = [ git cacert ];

  src = fetchFromGitHub {
    owner = "erlang-ls";
    repo = "erlang_ls";
    rev = "${version}";
    sha256 = "1jp4nrb4ns21jga7ysbqpwpkkmmsz90shcgk8qr4ibi4k0ly98ax";
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
