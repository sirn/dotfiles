# RVM
local rvm_script_path="$HOME/.rvm/scripts/rvm"
if [[ -e $rvm_script_path ]]; then
  source $rvm_script_path
fi

# Rbenv
_add_to_exec $HOME/.rbenv
if [[ -e $HOME/.rbenv ]]; then
  eval "$(rbenv init -)"
fi
