# RVM
_add_to_exec $HOME/.rbenv
if [[ -e $HOME/.rbenv ]]; then
  eval "$(rbenv init -)"
fi

if [[ -s "$HOME/.rvm/scripts/rvm" ]]; then
  source $HOME/.rvm/scripts/rvm
  export PATH=$PATH:$HOME/.rvm/bin
fi
