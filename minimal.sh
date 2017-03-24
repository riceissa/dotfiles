#!/bin/bash

cat <<'EOF' >> ~/.bashrc
# Modified from <https://www.jefftk.com/p/you-should-be-logging-shell-history>
promptFunc() {
    # right before prompting for the next command, save the previous
    # command in a file.
    echo "$(date -Iseconds) $(hostname) $PWD $(history 1)" \
        >> ~/.full_history
}
PROMPT_COMMAND=promptFunc

stty -ixon
EOF

cat <<'EOF' >> ~/.tmux.conf
set -s escape-time 0
unbind C-b
set -g prefix C-Space
bind C-Space send-prefix
EOF

curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

cat <<'EOF' >> ~/.vimrc
set nocompatible
call plug#begin('~/.vim/plugged')
Plug 'nelstrom/vim-visual-star-search'
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
if &history < 10000
  set history=10000
endif
set ignorecase smartcase showcmd noequalalways nojoinspaces
set spellfile=~/.spell.en.utf-8.add
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

" https://github.com/nelstrom/dotfiles/blob/448f710b855970a8565388c6665a96ddf4976f9f/vimrc#L80
cnoremap <expr> %% getcmdtype() == ':' ? fnameescape(expand('%:h')).'/' : '%%'

" https://github.com/tpope/tpope/blob/c743f64380910041de605546149b0575ed0538ce/.vimrc#L284
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
