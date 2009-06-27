set nocompatible                  " disable vi-compatibility
set backspace=indent,eol,start    " allow backspace over everything
set nobackup                      " everything is already in VCS

set history=50                    " keep 50 lines of command line history
set ruler                         " show the cursor position all the time
set showcmd                       " display incomplete commands
set textwidth=0                   " maximum width before auto break

syntax on                         " always color terminal anyway

set incsearch                     " do incremental searching
set hlsearch                      " highlight matched searchs

" auto-detect filetype and load language-specific indentations
filetype plugin indent on 
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

set autoindent                    " always enable auto indentation

set expandtab                     " insert spaces instead of tab
set tabstop=2                     " number of spaces for each tab
set shiftwidth=2                  " for autoindent
set smartindent                   " auto detect when to insert indentation

set list listchars=eol:¬          " display newline character as ¬
set ofu=syntaxcomplete#Complete   " enable Omni completion

" GUI-specified functions
if has("gui_running")
  colorscheme desert
  set guifont=Inconsolata:h14
endif

" Navigation
nmap <C-p> :tabprevious<cr>
nmap <C-n> :tabnext<cr>
nmap <C-t> :tabnew<cr>
nmap <C-d> :tabclose<cr>

" Filetype-specific settings
autocmd BufRead *.mako set filetype=mako
autocmd FileType mako setlocal shiftwidth=2
autocmd FileType mako setlocal tabstop=2
autocmd FileType mako setlocal syntax=mako
autocmd FileType mako setlocal omnifunc=htmlcomplete#CompleteTags

autocmd FileType python setlocal smartindent
  \ cinwords=if,elif,else,for,while,try,except,finally,def,class
autocmd FileType python setlocal shiftwidth=4
autocmd FileType python setlocal tabstop=4

" vim:ft=vim
