{ config, lib, ... }:

{
  home.file = {
    ".ideavimrc" = {
      text = ''
        " Plugins
        Plug 'tpope/vim-surround'
        Plug 'tpope/vim-commentary'

        " Defaults
        set scrolloff=5
        set clipboard+=unnamed
        set incsearch
        set ideajoin

        " Convenient mappings
        map Q gq
      '';
    };
  };
}
