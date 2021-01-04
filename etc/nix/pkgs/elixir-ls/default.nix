{ stdenv, fetchHex, fetchFromGitHub, beamPackages }:

with beamPackages;

let
  dialyxir = fetchHex {
    pkg = "dialyxir";
    version = "1.0.0";
    sha256 = "0m26cpw56gq1q47x1w7p5jy3sxaj5la1l1nq13519b2z2j46bc5f";
  };
  docsh = fetchHex {
    pkg = "docsh";
    version = "0.7.2";
    sha256 = "0d310wakhz9n9xs8bfwm0hnp35q17x8vhrnkqcmhsm07pdhv8zaf";
  };
  elixir_sense = fetchFromGitHub {
    owner = "elixir-lsp";
    repo = "elixir_sense";
    rev = "9c972fa7cc457b38e3dd49e9a7797a2ac9ef5eb3";
    sha256 = "09xi6ybk7fxw0l4a741hi9fj306rhk6i6ivxsacn3p3ygjrm3fhc";
  };
  erl2ex = fetchFromGitHub {
    owner = "dazuma";
    repo = "erl2ex";
    rev = "244c2d9ed5805ef4855a491d8616b8842fef7ca4";
    sha256 = "1z4fwcgxf847vffwnw4h6yallpnzjbn8ff6dvh53k7vr6h17l4vf";
  };
  erlex = fetchHex {
    pkg = "erlex";
    version = "0.2.6";
    sha256 = "0x8c1j62y748ldvlh46sxzv5514rpzm809vxn594vd7y25by5lif";
  };
  forms = fetchHex {
    pkg = "forms";
    version = "0.0.1";
    sha256 = "0s2qg5mn7d01h7sbx7fik5l4jdifrdlvsxgw8kvp38fmivnn63sk";
  };
  getopt = fetchHex {
    pkg = "getopt";
    version = "1.0.1";
    sha256 = "174mb46c2qd1f4a7507fng4vvscjh1ds7rykfab5rdnfp61spqak";
  };
  jason_vendored = fetchFromGitHub {
    owner = "elixir-lsp";
    repo = "jason";
    rev = "ee95ca80cd67b3a499a14f469536140935eb4483";
    sha256 = "025s2v8cc8r8qampgvhhafgj9z3z29qk06gmx2ww94qv7lg9f1jf";
  };
  mix_task_archive_deps = fetchFromGitHub {
    owner = "JakeBecker";
    repo = "mix_task_archive_deps";
    rev = "50301a4314e3cc1104f77a8208d5b66ee382970b";
    sha256 = "1mrk9hgalwz38x598w0r4pdgqyc2ra8n0qfpwyykh1gq1nwgp5s5";
  };
  providers = fetchHex {
    pkg = "providers";
    version = "1.8.1";
    sha256 = "183b9128l4af60rs40agqh6kc6db33j4027ad6jajxn4x6nlamz4";
  };
  stream_data = fetchHex {
    pkg = "stream_data";
    version = "0.5.0";
    sha256 = "0wg2p5hwf7qmkwsc1i3q7h558f7sr9f13y8i6kds9bb9q3pd4aq1";
  };
in
beamPackages.buildMix rec {
  name = "elixir-ls";
  version = "0.6.2";

  buildInputs = [ elixir rebar rebar3 ];

  src = fetchFromGitHub {
    owner =  "elixir-lsp";
    repo = "elixir-ls";
    rev = "v${version}";
    sha256 = "046c6jh8wqr33kq0ggvdmq6sjg07r2pqis1rpyjj94r5s9k9pn8a";
  };

  postPatch = ''
    mkdir -p deps

    cp --no-preserve=mode -R ${dialyxir} deps/dialyxir
    cp --no-preserve=mode -R ${docsh} deps/docsh
    cp --no-preserve=mode -R ${elixir_sense} deps/elixir_sense
    cp --no-preserve=mode -R ${erl2ex} deps/erl2ex
    cp --no-preserve=mode -R ${erlex} deps/erlex
    cp --no-preserve=mode -R ${forms} deps/forms
    cp --no-preserve=mode -R ${getopt} deps/getopt
    cp --no-preserve=mode -R ${stream_data} deps/stream_data
    cp --no-preserve=mode -R ${jason_vendored} deps/jason_vendored
    cp --no-preserve=mode -R ${mix_task_archive_deps} deps/mix_task_archive_deps
    cp --no-preserve=mode -R ${providers} deps/providers

    # TODO: remove these lines when this commit is included in elixir release:
    # https://github.com/elixir-lang/elixir/commit/535699f611a0730568d0a716250da5e0d86a609f
    cp --no-preserve=mode -R ${providers}/include/*.hrl deps/providers
    cp --no-preserve=mode -R ${docsh}/include/*.hrl deps/docsh

    # HACK: make mix believe all deps are cloned and latest
    for n in deps/*; do
      mkdir -p $n/.git
      touch $n/.git/HEAD
    done
  '';

  buildPhase = ''
    export HEX_OFFLINE=1
    export HEX_HOME=`pwd`
    export MIX_NO_DEPS=1
    export MIX_ENV=prod

    export MIX_REBAR=${rebar}/bin/rebar
    export MIX_REBAR3=${rebar3}/bin/rebar3

    mix deps.compile --no-deps-check --skip-umbrella-children
    mix compile --no-deps-check
  '';

  installPhase = ''
    export HEX_OFFLINE=1
    export HEX_HOME=`pwd`
    export MIX_NO_DEPS=1
    export MIX_ENV=prod

    mkdir -p $out/lib
    mkdir -p $out/bin

    # TODO: should probably patch mix_task_archive_deps to run archive.build
    # with --no-deps-check rather than relying on this hack
    for n in _build/shared/lib/*; do
      mix archive.build \
        --no-deps-check \
        -i $(pwd)/$n \
        -o $out/lib/$(basename $n).ez
    done

    substitute \
      $(pwd)/apps/elixir_ls_utils/priv/language_server.sh \
      $out/bin/elixir-ls \
      --replace 'exec "''${dir}/launch.sh"' "exec $out/lib/launch.sh"

    substitute \
      $(pwd)/apps/elixir_ls_utils/priv/debugger.sh \
      $out/lib/debugger.sh \
      --replace "ERL_LIBS=\"\$SCRIPTPATH:\$ERL_LIBS\"" "ERL_LIBS=$out/lib:\$ERL_LIBS" \
      --replace "elixir --erl" "${elixir}/bin/elixir --erl"

    substitute \
      $(pwd)/apps/elixir_ls_utils/priv/launch.sh \
      $out/lib/launch.sh \
      --replace "ERL_LIBS=\"\$SCRIPTPATH:\$ERL_LIBS\"" "ERL_LIBS=$out/lib:\$ERL_LIBS" \
      --replace "elixir --erl" "${elixir}/bin/elixir --erl"

    chmod +x $out/bin/elixir-ls
    chmod +x $out/lib/launch.sh
    chmod +x $out/lib/debugger.sh
  '';

  meta = {
    homepage = "https://github.com/elixir-lsp/elixir-ls";
    description = "A frontend-independent IDE smartness server for Elixir";
    license = stdenv.lib.licenses.asl20;
  };
}
