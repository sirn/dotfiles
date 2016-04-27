function ssh_reload_config
    cat $HOME/.ssh/config.d/* > $HOME/.ssh/config
end
