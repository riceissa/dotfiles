#!/bin/bash

# From http://www.jefftk.com/p/you-should-be-logging-shell-history
cat <<EOF >> ~/.bashrc
promptFunc() {
    # right before prompting for the next command, save the previous
    # command in a file.
    echo "$(date +%Y-%m-%d--%H-%M-%S) $(hostname) $PWD $(history 1)" \
        ~/.full_history
}
PROMPT_COMMAND=promptFunc

stty -ixon
EOF

cat <<EOF >> ~/.tmux.conf
set -s escape-time 0
EOF

curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

cat <<EOF >> ~/.vimrc
call plug#begin('~/.vim/plugged')
Plug 'riceissa/vim-emacsctrll'
Plug 'tpope/vim-characterize'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
call plug#end()

" Workaround for https://github.com/tpope/vim-sleuth/issues/29 to override
" sleuth.vim for some filetypes.
runtime! plugin/sleuth.vim

set number nohlsearch
set ignorecase smartcase showcmd noequalalways nojoinspaces
set spellfile=~/.spell.en.add
set wildmode=list:longest,full

nnoremap Y y$
nnoremap K <C-^>
if has('nvim') && maparg('<Leader>K', 'n') ==# ''
  noremap <Leader>K :Man<CR>
endif

digraph el 8230
digraph ./ 8230
digraph ^\| 8593
digraph \\ 8726
digraph \- 8726
digraph -\ 8726
digraph \|> 8614
" Run under exe so that syntax highlighting isn't messed up
exe 'digraph (/ 8713'
exe 'digraph (\ 8713'
exe 'digraph (< 10216'
exe 'digraph >) 10217'

cnoremap <expr> %% getcmdtype() == ':' ? fnameescape(expand('%:h')).'/' : '%%'

inoremap <C-G><C-T> <C-R>=<SID>ListDate()<CR>
function! s:ListDate()
    let date_fmts = [
          \ "%F",
          \ "%B %-d, %Y",
          \ "%-d %B %Y",
          \ "%Y-%m-%d %H:%M:%S",
          \ "%a, %d %b %Y %H:%M:%S %z",
          \ "%Y %b %d",
          \ "%d-%b-%y",
          \ "%a %b %d %T %Z %Y"
          \ ]
    let compl_lst = map(date_fmts, 'strftime(v:val)') + [localtime()]
    call complete(col('.'), compl_lst)
    return ''
endfunction

let g:tex_flavor='latex'
EOF
