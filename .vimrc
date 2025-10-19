set nocompatible
if !has('nvim')
  if !(exists('g:did_load_filetypes') && exists('g:did_load_ftplugin') && exists('g:did_indent_on'))
    filetype plugin indent on
  endif
  if has('syntax') && !exists('g:syntax_on') && (&t_Co > 2 || has("gui_running"))
    syntax enable
  endif
  if !empty(globpath(&packpath, 'pack/*/opt/editorconfig'))
    packadd! editorconfig
  endif
  if !empty(globpath(&packpath, 'pack/*/opt/comment'))
    packadd! comment
  endif
  runtime macros/matchit.vim
  runtime ftplugin/man.vim
  if exists(':Man') == 2
    set keywordprg=:Man
  endif
endif

" Options that even Vim now sets in C code, but are required in older Vim
" versions
set ruler showcmd backspace=indent,eol,start wildmenu

" Copying some of the Neovim defaults that I like
set ttimeout ttimeoutlen=50 hidden nostartofline nojoinspaces autoindent
set laststatus=2 display=lastline nrformats-=octal complete-=i
silent! while 0
  set history=10000
silent! endwhile
if &history < 10000
  set history=10000
endif
silent! while 0
  silent! set belloff=all
silent! endwhile
if has('patch-7.4.793')
  set belloff=all
endif
if has('reltime')
  set incsearch
endif
if has('path_extra')
  set tags=./tags;,tags
endif
silent! while 0
  silent! set mouse=nvi
silent! endwhile
if has('mouse')
  set mouse=nvi
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

setglobal expandtab shiftwidth=4 softtabstop=4
set viminfo& scrolloff=3 completeopt=menu
set nohlsearch ignorecase smartcase shortmess-=S
set listchars=tab:>-,trail:@,extends:>,precedes:<,nbsp:+
set formatoptions=tcrq
if v:version > 703 || (v:version == 703 && has('patch541'))
  set formatoptions+=j
endif
if has('clipboard') && (v:version > 703 || (v:version == 703 && has('patch074')))
  set clipboard^=unnamedplus
endif
if exists('+smoothscroll')
  set smoothscroll
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

if has('patch-7.4.513')
  function! s:LinewisePasteOp(type) abort
    let temp = @s
    call setreg('s', getreg(v:register, 1, 1), 'l')
    let l:do_after_paste = (s:post_paste ==# '' ? '' : s:post_paste . "']")
    exe 'normal! ' . v:count1 . '"s]' . s:paste_command . l:do_after_paste
    let @s = temp
  endfunction
  function! s:Paste(paste_command, post_paste) abort
    let s:paste_command = a:paste_command
    let s:post_paste = a:post_paste
    let &operatorfunc = matchstr(expand('<sfile>'), '<SNR>\d\+_') . 'LinewisePasteOp'
    return 'g@l'
  endfunction
  " These mappings are from unimpaired.vim
  nnoremap <expr> >p <SID>Paste("p", ">")
  nnoremap <expr> >P <SID>Paste("P", ">")
  nnoremap <expr> <p <SID>Paste("p", "<")
  nnoremap <expr> <P <SID>Paste("P", "<")
  nnoremap <expr> =p <SID>Paste("p", "=")
  nnoremap <expr> =P <SID>Paste("P", "=")
  nnoremap <expr> ]p <SID>Paste("p", "")
  nnoremap <expr> ]P <SID>Paste("P", "")
  nnoremap <expr> [p <SID>Paste("P", "")
  nnoremap <expr> [P <SID>Paste("P", "")
endif

if !has('nvim-0.11')
  function! s:BlankLinesOp(type) abort
    call append(line('.') + s:line_offset, repeat([''], v:count1))
  endfunction
  function! s:InsertBlankLines(line_offset) abort
    let s:line_offset = a:line_offset
    let &operatorfunc = matchstr(expand('<sfile>'), '<SNR>\d\+_') . 'BlankLinesOp'
    return 'g@l'
  endfunction
  nnoremap <expr> ]<Space> <SID>InsertBlankLines(0)
  nnoremap <expr> [<Space> <SID>InsertBlankLines(-1)

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
    autocmd FileType vim if &keywordprg ==# '' | setlocal keywordprg=:help | endif
    if empty(globpath(&packpath, 'pack/*/opt/editorconfig'))
      autocmd FileType vim setlocal expandtab shiftwidth=2 softtabstop=2
    endif
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
