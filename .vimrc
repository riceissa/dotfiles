set nocompatible
if !has('nvim')
  filetype plugin indent on
  if has('syntax') && (&t_Co > 2 || has("gui_running"))
    syntax on
  endif

  silent! packadd! editorconfig
  silent! packadd! comment
  silent! packadd! matchit
  runtime ftplugin/man.vim
  if exists(':Man') == 2
    set keywordprg=:Man
  endif
endif

set ttimeout ttimeoutlen=50
set display=lastline
if has('langmap') && exists('+langremap')
  set nolangremap
endif
set nrformats-=octal
set laststatus=2
silent! while 0
  set history=1000
silent! endwhile
if &history < 1000
  set history=1000
endif
set viminfo&
silent! set belloff=all
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
set hidden
set nostartofline
set nojoinspaces
set autoindent
set formatoptions=tcrqj

if has('reltime')
  set incsearch
endif
set nohlsearch
set ignorecase smartcase
set shortmess-=S

if has('clipboard')
  set clipboard^=unnamedplus
endif

silent! while 0
  silent! set mouse=nv
silent! endwhile
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

silent! while 0
  silent! set wildoptions=pum,tagfile
silent! endwhile
if has('nvim') || has('patch-8.2.4325')
  set wildoptions=pum,tagfile
else
  set wildoptions=tagfile
endif
if has('path_extra')
  set tags=./tags;,tags
endif

nnoremap Y y$
if 1
  nnoremap <expr> j v:count > 0 \|\| &filetype ==# 'qf' ? 'j' : 'gj'
  nnoremap <expr> k v:count > 0 \|\| &filetype ==# 'qf' ? 'k' : 'gk'
  xnoremap <expr> j mode() ==# 'V' \|\| mode() ==# "\<C-V>" \|\| v:count > 0 ? 'j' : 'gj'
  xnoremap <expr> k mode() ==# 'V' \|\| mode() ==# "\<C-V>" \|\| v:count > 0 ? 'k' : 'gk'
endif
inoremap <C-A> <C-O>^
inoremap <C-X><C-A> <C-A>
cnoremap <C-A> <Home>
cnoremap <C-X><C-A> <C-A>
silent! while 0
  inoremap <C-E> <End>
  inoremap <C-F> <Right>
  cnoremap <C-F> <Right>
silent! endwhile
if 1
  inoremap <expr> <C-E> col(".") >= col("$") ? "<C-E>" : "<End>"
  inoremap <expr> <C-F> col(".") >= col("$") ? "<C-F>" : "<Right>"
  cnoremap <expr> <C-F> getcmdpos() > strlen(getcmdline()) ? &cedit : "<Right>"
endif
inoremap <C-B> <Left>
cnoremap <C-B> <Left>
silent! while 0
  inoremap <C-D> <Del>
silent! endwhile
if 1
  inoremap <expr> <C-D> col(".") >= col("$") ? "<C-D>" : "<Del>"
  cnoremap <expr> <C-D> getcmdpos() > strlen(getcmdline()) ? "<C-D>" : "<Del>"
endif
inoremap <C-W> <C-G>u<C-W>
inoremap <C-U> <C-G>u<C-U>

if !has('nvim-0.8.0')
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

if !has('nvim')
  nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
endif

if !has('nvim-0.11')
  nnoremap <silent> ]<Space> :<C-U>call append(line('.'), repeat([''], v:count1))<CR>
  nnoremap <silent> [<Space> :<C-U>call append(line('.')-1, repeat([''], v:count1))<CR>
  nnoremap <silent> ]q :cnext<CR>
  nnoremap <silent> [q :cprevious<CR>
endif

if exists('*strftime')
  function! s:CompleteDateTime()
    let date_formats = ['%Y-%m-%d', '%B %-d, %Y', '%B %Y', '%Y-%m-%d %H:%M:%S']
    call complete(col('.'), map(date_formats, 'strftime(v:val)') + [localtime()])
    return ''
  endfunction
  inoremap <C-G><C-T> <C-R>=<SID>CompleteDateTime()<CR>
endif

silent! while 0
  cnoremap %% %:h/<C-L>
silent! endwhile
if 1
  cnoremap %% <C-R><C-R>=getcmdtype() == ':' ? fnameescape(expand('%:h')).'/' : '%%'<CR>
endif

nnoremap g/ /[^\d32-\d126]<CR>

inoreabbrev ADd Add

set cinoptions=l1
if 1
  let c_no_curly_error = 1

  let g:python_indent = {}
  let g:python_indent.open_paren = 'shiftwidth()'
  if has('patch-7.4.1154')
    let g:python_indent.closed_paren_align_last_line = v:false
  endif
endif

if exists(":DiffOrig") != 2
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_
        \ | diffthis | wincmd p | diffthis
endif

if has('autocmd')
  augroup vimrc
    autocmd!
    if !has('nvim') && !has('patch-8.2.3464')
      autocmd BufNewFile,BufRead /etc/nginx/* setfiletype nginx
    endif
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

  if exists('#fedora')
    autocmd! fedora
  endif

  augroup RestoreCursor
    autocmd!
    autocmd BufReadPost *
          \ let line = line("'\"")
          \ | if line >= 1 && line <= line("$") && &filetype !~# 'commit'
          \      && index(['xxd', 'gitrebase'], &filetype) == -1
          \      && !&diff
          \ |   execute "normal! g`\""
          \ | endif
  augroup END
endif
