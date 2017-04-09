" Dump the values of all settings that either sensible.vim or Neovim alters.
" Use as follows:
"    vim -c 'source dump_union_settings.vim|write out.txt|quit'
redir @a
filetype
if has('syntax') && exists('g:syntax_on')
  echo "syntax: " . g:syntax_on
else
  echo "syntax: 0"
endif

set autoindent?
set autoread?
set backspace?
set backupdir?
set complete?
set directory?
set display?
set encoding?
set formatoptions?
set history?
set hlsearch?
set incsearch?
set langnoremap?
set laststatus?
set listchars?
set mouse?
set nocompatible?
set nrformats?
set ruler?
set scrolloff?
set sessionoptions?
set shell?
set sidescrolloff?
set smarttab?
set tabpagemax?
set tags?
set ttimeout?
set ttimeoutlen?
set ttyfast?
set undodir?
set viminfo?
set wildmenu?

nmap <C-L>
imap <C-U>
redir END
put a

" Remove blank lines
global/^$/d
