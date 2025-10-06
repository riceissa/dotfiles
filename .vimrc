set nocompatible
if !has('nvim')
  unlet! skip_defaults_vim
  source $VIMRUNTIME/defaults.vim

  silent! packadd! editorconfig
  silent! packadd! comment
  runtime ftplugin/man.vim
  set keywordprg=:Man
endif

set ttimeout ttimeoutlen=50
set laststatus=2
if &history < 1000
  set history=1000
endif
set viminfo&
set belloff=all
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
set hidden
set nostartofline
set nojoinspaces
set autoindent
set formatoptions=tcrqj

set nohlsearch
set ignorecase smartcase
set shortmess-=S

if has('clipboard')
  set clipboard^=unnamedplus
endif

if has('mouse')
  set mouse=nv
  if !has('nvim') && exists('$TMUX')
    set ttymouse=xterm2
  endif
endif

if exists('+smoothscroll')
  set smoothscroll
endif
set scrolloff=2

if has('nvim') || has('patch-8.2.4325')
  set wildoptions=pum,tagfile
else
  set wildoptions=tagfile
endif
set tags=./tags;,tags

nnoremap Y y$
nnoremap <expr> j v:count > 0 ? 'j' : 'gj'
nnoremap <expr> k v:count > 0 ? 'k' : 'gk'
xnoremap <expr> j mode() ==# 'V' \|\| mode() ==# "\<C-V>" \|\| v:count > 0 ? 'j' : 'gj'
xnoremap <expr> k mode() ==# 'V' \|\| mode() ==# "\<C-V>" \|\| v:count > 0 ? 'k' : 'gk'
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
inoremap <C-W> <C-G>u<C-W>

if !has('nvim')
  function! s:VisualStarSearch()
    let temp = @s
    normal! gv"sy
    let search = '\V' . substitute(escape(@s, '\'), '\n', '\\n', 'g')
    call setreg('/', search)
    call histadd('/', search)
    let @s = temp
  endfunction
  xnoremap * :<C-U>call <SID>VisualStarSearch()<CR>/<CR>
  xnoremap # :<C-U>call <SID>VisualStarSearch()<CR>?<CR>
endif

set cinoptions=l1
if 1
  unlet! c_comment_strings
  let c_no_curly_error = 1

  let g:python_indent = {}
  let g:python_indent.open_paren = 'shiftwidth()'
  let g:python_indent.closed_paren_align_last_line = v:false
endif

if has('autocmd')
  augroup vimrc
    autocmd!
    autocmd BufNewFile,BufRead /etc/nginx/* setfiletype nginx
    autocmd FileType * set formatoptions-=o
    autocmd FileType haskell syntax match hsLineComment '^#!.*'
    autocmd FileType go,rust setlocal formatprg=
    autocmd FileType rust if &makeprg =~# '^rustc ' | setlocal makeprg=make | endif
    autocmd FileType rust setlocal textwidth=0
    autocmd FileType markdown setlocal iskeyword-=_
    autocmd FileType gitcommit setlocal spell
    autocmd FileType c,php,glsl setlocal commentstring=//\ %s
    autocmd FileType vim setlocal commentstring=\"\ %s
    autocmd FileType kitty setlocal commentstring=#\ %s

    if has('nvim-0.10')
      autocmd OptionSet background
        \   if &background ==# 'light'
        \ |   colorscheme vim
        \ |   set notermguicolors
        \ | else
        \ |   colorscheme default
        \ |   set termguicolors
        \ | endif
        \ | let &ft = &ft
    endif
  augroup END

  if exists('#vimHints')
    autocmd! vimHints
  endif

  if exists('#fedora')
    autocmd! fedora
  endif

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
