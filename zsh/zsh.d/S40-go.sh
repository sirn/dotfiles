if [[ -n `brew 2>/dev/null` ]]; then
  export GOROOT=`brew --prefix go`
fi
