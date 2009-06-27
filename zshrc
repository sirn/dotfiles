# System detection
if [[ ( -n ${OSTYPE:#darwin*} && -n ${OSTYPE:#*bsd*} ) ]]; then
else
  export is_bsd=1
fi

# Default config
. ~/.dotfiles/zsh/config
. ~/.dotfiles/zsh/aliases

# zsh.d
for zshrc_snipplet in ~/.dotfiles/zsh/zsh.d/S[0-9][0-9]*[^~] ; do
  source $zshrc_snipplet
done
