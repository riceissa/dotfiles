if !has('nvim')
  unlet! skip_defaults_vim
  source $VIMRUNTIME/defaults.vim
  silent! packadd! editorconfig
  silent! packadd! comment
endif

set ttimeout ttimeoutlen=50
set nohlsearch ignorecase smartcase
set scrolloff=0
set laststatus=2
set hidden
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
set belloff=all
set autoindent
if has('mouse')
  set mouse=nv
endif
nnoremap Y y$

inoremap <C-A> <C-O>^
inoremap <C-X><C-A> <C-A>
cnoremap <C-A> <Home>
cnoremap <C-X><C-A> <C-A>
inoremap <expr> <C-E> col(".") >= col("$") ? "<C-E>" : "<End>"
inoremap <expr> <C-F> col(".") >= col("$") ? "<C-F>" : "<Right>"
cnoremap <expr> <C-F> getcmdpos() > strlen(getcmdline()) ? &cedit : "<Right>"
inoremap <C-B> <Left>
cnoremap <C-B> <Left>
inoremap <expr> <C-D> col(".") >= col("$") ? "<C-D>" : "<Del>"
cnoremap <expr> <C-D> getcmdpos() > strlen(getcmdline()) ? "<C-D>" : "<Del>"

if has('autocmd')
  augroup vimrc
    autocmd!
    autocmd BufNewFile,BufRead /etc/nginx/* setfiletype nginx
    autocmd FileType haskell syntax match hsLineComment '^#!.*'
    autocmd FileType markdown setlocal iskeyword-=_
    autocmd FileType gitcommit setlocal spell
  augroup END

  " From :help restore-cursor on Neovim. Vim already has this in defaults.vim.
  if has('nvim')
    augroup RestoreCursor
      autocmd!
      autocmd BufReadPre * autocmd FileType <buffer> ++once
        \ let s:line = line("'\"")
        \ | if s:line >= 1 && s:line <= line("$") && &filetype !~# 'commit'
        \      && index(['xxd', 'gitrebase'], &filetype) == -1
        \ |   execute "normal! g`\""
        \ | endif
    augroup END
  endif
endif
