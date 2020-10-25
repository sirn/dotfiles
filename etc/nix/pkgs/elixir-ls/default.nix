{ stdenv, fetchHex, fetchFromGitHub, beamPackages, rebar3, git, cacert }:

with beamPackages;

# TODO: prevent fetch during build.
beamPackages.buildMix rec {
  name = "elixir-ls-${version}";
  version = "0.6.0";

  buildInputs = [ elixir rebar3 git cacert ];

  src = fetchFromGitHub {
    owner =  "elixir-lsp";
    repo = "elixir-ls";
    rev = "v${version}";
    sha256 = "0md7nr02ba3gs2xr9zwqvhjgxr7s0lg91sq8a95v07mjp1jgmpgc";
  };

  buildPhase = ''
    export HEX_HOME="$TMPDIR/hex"
    export MIX_HOME="$TMPDIR/mix"
    export MIX_ENV=prod
    export MIX_REBAR=${rebar}/bin/rebar
    export MIX_REBAR3=${rebar3}/bin/rebar3

    mix deps.get
    mix compile
    mix elixir_ls.release
  '';

  installPhase = ''
    mkdir -p $out/lib
    mkdir -p $out/bin

    cp -Rv release/*.ez $out/lib/

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
