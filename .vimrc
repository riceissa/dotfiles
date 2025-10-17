set nocompatible
if !has('nvim')
  filetype plugin indent on
  if has('syntax') && !exists('g:syntax_on') && (&t_Co > 2 || has("gui_running"))
    syntax enable
  endif

  silent! packadd! editorconfig
  silent! packadd! comment
  runtime! macros/matchit.vim
  runtime ftplugin/man.vim
  if exists(':Man') == 2
    set keywordprg=:Man
  endif
endif

set ttimeout ttimeoutlen=50 hidden nostartofline nojoinspaces autoindent
set viminfo& display=lastline nrformats-=octal laststatus=2
set complete-=i scrolloff=3 completeopt=menu
set formatoptions=tcrq
if v:version > 703 || (v:version == 703 && has('patch541'))
  set formatoptions+=j
endif
set nohlsearch ignorecase smartcase shortmess-=S
set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
silent! set belloff=all
if has('reltime')
  set incsearch
endif
if has('clipboard') && (v:version > 703 || (v:version == 703 && has('patch074')))
  set clipboard^=unnamedplus
endif
if exists('+smoothscroll')
  set smoothscroll
endif
if has('path_extra')
  set tags=./tags;,tags
endif

silent! while 0
  set history=1000
silent! endwhile
if &history < 1000
  set history=1000
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

silent! while 0
  set wildoptions=tagfile
  silent! set wildoptions=pum,tagfile
silent! endwhile
if has('nvim') || has('patch-8.2.4325')
  set wildoptions=pum,tagfile
else
  set wildoptions=tagfile
endif

nnoremap Y y$
nnoremap g/ /[^\d32-\d126]<CR>
inoremap <C-U> <C-G>u<C-U>
inoremap <C-W> <C-G>u<C-W>
if 1
  nnoremap <expr> j v:count > 0 \|\| &filetype ==# 'qf' ? 'j' : 'gj'
  nnoremap <expr> k v:count > 0 \|\| &filetype ==# 'qf' ? 'k' : 'gk'
  xnoremap <expr> j mode() ==# 'V' \|\| mode() ==# "\<C-V>" \|\| v:count > 0 ? 'j' : 'gj'
  xnoremap <expr> k mode() ==# 'V' \|\| mode() ==# "\<C-V>" \|\| v:count > 0 ? 'k' : 'gk'
endif

" See :help emacs-keys. These particular mappings are mostly from rsi.vim.
inoremap <C-A> <C-O>^
inoremap <C-X><C-A> <C-A>
cnoremap <C-A> <Home>
cnoremap <C-X><C-A> <C-A>
inoremap <C-B> <Left>
cnoremap <C-B> <Left>
silent! while 0
  inoremap <C-D> <Del>
  inoremap <C-E> <End>
  inoremap <C-F> <Right>
silent! endwhile
if 1
  inoremap <expr> <C-D> col(".") >= col("$") ? "<C-D>" : "<Del>"
  cnoremap <expr> <C-D> getcmdpos() > strlen(getcmdline()) ? "<C-D>" : "<Del>"
  inoremap <expr> <C-E> col(".") >= col("$") ? "<C-E>" : "<End>"
  inoremap <expr> <C-F> col(".") >= col("$") ? "<C-F>" : "<Right>"
  cnoremap <expr> <C-F> getcmdpos() > strlen(getcmdline()) ? &cedit : "<Right>"
endif

" See https://github.com/riceissa/vim-pasteurize/blob/3a80557a45c684c7cf5f0ff85effaef925b59381/plugin/pasteurize.vim#L10-L65
" See also
" https://github.com/tpope/vim-unimpaired/blob/db65482581a28e4ccf355be297f1864a4e66985c/doc/unimpaired.txt#L100-L112
" I decided not to add all those mappings from unimpaired because they break
" the dot command unless I also install repeat.vim, and I don't want to install
" any plugins. With ]p , the last native Vim command *is* the paste, so the dot
" command still works as expected. Instead of adding a bunch of other mappings,
" I decided to make it visually select the pasted text, so I can just gv to
" re-select the text, and then I can > or < or = from there if I want to.
if has('patch-7.4.513')
  function! s:PreparePaste(current_reg)
    let l:reg = getreg(a:current_reg, 1, 1)
    if a:current_reg ==# '+'
      while !empty(l:reg) && l:reg[-1] ==# ''
        call remove(l:reg, -1)
      endwhile
      while !empty(l:reg) && l:reg[0] ==# ''
        call remove(l:reg, 0)
      endwhile
    endif
    call setreg(a:current_reg, l:reg, 'l')
  endfunction
  nnoremap <expr> ]p ":<C-U>call <SID>PreparePaste(v:register)<CR>" . v:count1 . '"' . v:register . "]pV']o<Esc>"
  nnoremap <expr> ]P ":<C-U>call <SID>PreparePaste(v:register)<CR>" . v:count1 . '"' . v:register . "]PV']o<Esc>"
  nmap [p ]P
  nmap [P ]P
endif

if !has('nvim-0.8.0')
  " Modified from https://github.com/nelstrom/vim-visual-star-search
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
  " From sensible.vim
  nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
endif

if !has('nvim-0.11')
  nnoremap <silent> ]<Space> :<C-U>call append(line('.'), repeat([''], v:count1))<CR>
  nnoremap <silent> [<Space> :<C-U>call append(line('.')-1, repeat([''], v:count1))<CR>
  nnoremap <expr><silent> ]q ":<C-U>" . v:count1 . "cnext<CR>"
  nnoremap <expr><silent> [q ":<C-U>" . v:count1 . "cprevious<CR>"
endif

if exists('*strftime')
  " Modified from https://github.com/tpope/dotfiles/blob/c743f64380910041de605546149b0575ed0538ce/.vimrc#L284
  function! s:CompleteDateTime()
    let date_formats = ['%Y-%m-%d', '%B %-d, %Y', '%B %Y', '%Y-%m-%d %H:%M:%S']
    call complete(col('.'), map(date_formats, 'strftime(v:val)') + [localtime()])
    return ''
  endfunction
  inoremap <C-G><C-T> <C-R>=<SID>CompleteDateTime()<CR>
endif

" First seen at http://vimcasts.org/episodes/the-edit-command/ but this
" particular version is modified from
" https://github.com/nelstrom/dotfiles/blob/448f710b855970a8565388c6665a96ddf4976f9f/vimrc#L80
silent! while 0
  cnoremap %% %:h/<C-L>
silent! endwhile
if 1
  cnoremap %% <C-R><C-R>=getcmdtype() == ':' ? fnameescape(expand('%:h')).'/' : '%%'<CR>
endif

set cinoptions=l1
if 1
  unlet! c_comment_strings
  let c_no_curly_error = 1

  let g:python_indent = {}
  let g:python_indent.open_paren = 'shiftwidth()'
  if has('patch-7.4.1154')
    let g:python_indent.closed_paren_align_last_line = v:false
  endif
endif

" See :help :DiffOrig
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
    autocmd FileType vim setlocal textwidth=0 commentstring=\"\ %s
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

  if exists('#fedora#BufReadPost *')
    autocmd! fedora BufReadPost *
  endif

  " See :help restore-cursor
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
