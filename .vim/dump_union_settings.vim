" Dump the values of all settings that either sensible.vim or Neovim alters.
" Use as follows:
"    vim -c 'source dump_union_settings.vim|write out.txt|quit'
" The idea is to generate dumps from both Vim and Neovim and compare them and
" eliminate differences to create a more consistent experience.
function Out()
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
set belloff?
set complete?
set directory?
set display?
set encoding?
set formatoptions?
set history?
set hlsearch?
set incsearch?
set langnoremap?
set langremap?
set laststatus?
set listchars?
set mouse?
set nocompatible?
set nrformats?
set ruler?
set scrolloff?
set sessionoptions?
set shell?
set showcmd?
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

echo "t_Co=" . &t_Co
if exists('g:loaded_matchit')
  echo "matchit: " . g:loaded_matchit
else
  echo "matchit: 0"
endif
endfunction

redir @a
silent call Out()
redir END
put a

" Remove blank lines
global/^$/d
