{ stdenv, bundlerEnv, ruby, makeWrapper }:

stdenv.mkDerivation rec {
  name = "tmuxinator-${env.gems.tmuxinator.version}";

  env = bundlerEnv {
    inherit ruby;
    name = "${name}-gems";
    gemfile = ./Gemfile;
    lockfile = ./Gemfile.lock;
    gemset = ./gemset.nix;
  };

  buildInputs = [ makeWrapper ];
  phases = ["installPhase"];
  installPhase = ''
    mkdir -p $out/bin
    makeWrapper ${env}/bin/tmuxinator $out/bin/tmuxinator
  '';

  meta = with stdenv.lib; {
    description = "Manage complex tmux sessions easily";
    homepage = https://github.com/tmuxinator/tmuxinator;
    license = licenses.mit;
    platforms = platforms.unix;
  };
}