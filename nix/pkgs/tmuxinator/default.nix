{ stdenv, bundlerEnv, ruby }:

bundlerEnv rec {
  name = "tmuxinator-${version}";
  version = "0.8.1";

  inherit ruby;
  gemfile = ./Gemfile;
  lockfile = ./Gemfile.lock;
  gemset = ./gemset.nix;

  meta = with stdenv.lib; {
    description = "Manage complex tmux sessions easily";
    homepage = https://github.com/tmuxinator/tmuxinator;
    license = licenses.mit;
    maintainers = with maintainers; [ auntie ];
    platforms = platforms.unix;
  };
}