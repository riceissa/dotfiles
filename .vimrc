" Allow this vimrc file to be interpreted without errors in vim-tiny and other
" minimal Vim versions; see
" https://github.com/riceissa/computing-notes/blob/main/vim.md#why-set-nocompatible
" for a full explanation.
set nocompatible

if !(exists('g:did_load_filetypes') && exists('g:did_load_ftplugin') && exists('g:did_indent_on'))
  " This is required even for Neovim, in order to make the RestoreCursor
  " autocommand below work properly. See
  " https://github.com/neovim/neovim/issues/15536#issuecomment-909336062 for
  " details.
  filetype plugin indent on
endif

if has('syntax') && !exists('g:syntax_on') && (&t_Co > 2 || has("gui_running"))
  syntax enable
endif
if exists('+packpath') && !empty(globpath(&packpath, 'pack/*/opt/editorconfig'))
  packadd! editorconfig
endif
if exists('+packpath') && !empty(globpath(&packpath, 'pack/*/opt/comment'))
  packadd! comment
endif
if exists('+packpath') && !empty(globpath(&packpath, 'pack/*/opt/helptoc'))
  packadd! helptoc
  nnoremap gO <Cmd>HelpToc<CR>
endif

if !has('nvim')
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
set ttimeout ttimeoutlen=50 nojoinspaces autoindent
set hidden  " See https://github.com/riceissa/computing-notes/blob/main/vim.md#why-set-hidden
set nostartofline  " See https://github.com/riceissa/computing-notes/blob/main/vim.md#why-nostartofline
set history=10000 laststatus=2 display=lastline nrformats-=octal complete-=i
silent! set belloff=all
silent! set incsearch
silent! set tags=./tags;,tags  " See https://github.com/riceissa/computing-notes/blob/main/vim.md#working-with-tags
silent! set mouse=nvi
if has('mouse') && exists('&ttymouse') && exists('$TMUX')
  set ttymouse=xterm2  " See https://github.com/riceissa/computing-notes/blob/main/vim.md#making-the-mouse-work-in-vim-under-tmux
endif
set wildoptions=tagfile
silent! set wildoptions=pum,tagfile  " See https://github.com/riceissa/computing-notes/blob/main/vim.md#wildoptions

set expandtab shiftwidth=4 softtabstop=4  " In case EditorConfig is not available
set viminfo&  " Fedora's /etc/vimrc sets this to a terrible value, so reset it to the Vim default; see https://github.com/riceissa/computing-notes/blob/main/vim.md#vimrc-on-fedora for more information.
set scrolloff=3 completeopt=menu guicursor=
set nohlsearch ignorecase smartcase
set shortmess-=S  " Show number of matches when searching
set listchars=tab:>-,extends:>,precedes:<,nbsp:+,trail:@
set formatoptions=tcrq
silent! set formatoptions+=j
silent! set clipboard^=unnamedplus
silent! set smoothscroll
" Make files without extensions get lower priority when tab-completing. A file
" without an extension is usually an executable, e.g. prog when prog.c exists,
" so this effectively means prioritizing the source files.
set suffixes+=,
" And a few more extensions I am unlikely to want to open in Vim:
set suffixes+=.pdf,.epub,.ttf

nnoremap Y y$
nnoremap g/ /[^\d32-\d126]<CR>

" Mapping using C-/ works on kitty but not Gnome Terminal. Mapping using C-_
" works on Gnome Terminal (and the mapping can be activated using both C-_ and
" C-/), but on kitty that mapping does not work at all (pressing C-/ just acts
" as if I typed /, and pressing C-_ makes the font size smaller). There might
" also be a Vim vs Neovim thing going on. Having both mappings here makes it
" work in all the terminal + Vim/Neovim + tmux configurations I've tried.
if 1
  nnoremap <expr> <C-/> ":<C-U>set " . (&hlsearch ? "nohlsearch" : "hlsearch") . "<CR>"
  nnoremap <expr> <C-_> ":<C-U>set " . (&hlsearch ? "nohlsearch" : "hlsearch") . "<CR>"
endif

inoremap <C-U> <C-G>u<C-U>
inoremap <C-W> <C-G>u<C-W>
if 1
  " See https://github.com/riceissa/computing-notes/blob/main/vim.md#better-j-and-k
  " for explanation.
  nnoremap <expr> j v:count > 0 <Bar><Bar> &filetype ==# 'qf' ? 'j' : 'gj'
  nnoremap <expr> k v:count > 0 <Bar><Bar> &filetype ==# 'qf' ? 'k' : 'gk'
  xnoremap <expr> j mode() ==# 'V' <Bar><Bar> mode() ==# "\<C-V>" <Bar><Bar> v:count > 0 ? 'j' : 'gj'
  xnoremap <expr> k mode() ==# 'V' <Bar><Bar> mode() ==# "\<C-V>" <Bar><Bar> v:count > 0 ? 'k' : 'gk'
endif

" See :help emacs-keys. These particular mappings are mostly from rsi.vim.
" See https://github.com/riceissa/computing-notes/blob/main/vim.md#rsivim
" for more explanation.
inoremap <C-A> <C-O>^
inoremap <C-X><C-A> <C-G>u<C-A>
cnoremap <C-A> <Home>
cnoremap <C-X><C-A> <C-A>
inoremap <C-B> <Left>
cnoremap <C-B> <Left>
silent! while 0
  inoremap <C-D> <Del>
  inoremap <C-E> <End>
  inoremap <C-F> <Right>
  cnoremap <C-F> <Right>
  cnoremap <C-X><C-E> <C-F>
silent! endwhile
if 1
  inoremap <expr> <C-D> col(".") >= col("$") ? "<C-D>" : "<Del>"
  cnoremap <expr> <C-D> getcmdpos() > strlen(getcmdline()) ? "<C-D>" : "<Del>"
  inoremap <expr> <C-E> col(".") >= col("$") ? "<C-E>" : "<End>"
  inoremap <expr> <C-F> col(".") >= col("$") ? "<C-F>" : "<Right>"
  cnoremap <expr> <C-F> getcmdpos() > strlen(getcmdline()) ? &cedit : "<Right>"
  cnoremap <expr> <C-X><C-E> &cedit
endif

if maparg('*', 'x') ==# ''
  " Modified from https://github.com/nelstrom/vim-visual-star-search
  " See https://github.com/riceissa/computing-notes/blob/main/vim.md#visual-star-search-for-vim
  " for more explanation of this implementation.
  function! s:VisualStarSearch() abort
    let s_type = getregtype('s')
    if has('patch-7.4.513')
      let s_contents = getreg('s', 1, 1)
    else
      let s_contents = getreg('s')
    endif
    normal! gv"sy
    let search = '\V' . substitute(escape(@s, '\'), '\n', '\\n', 'g')
    call setreg('/', search)
    call histadd('/', search)
    call setreg('s', s_contents, s_type)
  endfunction
  xnoremap * :<C-U>call <SID>VisualStarSearch()<CR>/<CR>
  xnoremap # :<C-U>call <SID>VisualStarSearch()<CR>?<CR>
endif

if 1
  function! s:EmacsCtrlL() abort
    if abs(winline()) <= 1+&scrolloff
      return "zb"
    elseif abs(winline() - (1+winheight(0))/2) <= 1
      return "zt"
    else
      return "zz"
    endif
  endfunction
  inoremap <expr> <C-L> &insertmode <Bar><Bar> pumvisible() ? "<C-L>" : '<C-\><C-O>' . <SID>EmacsCtrlL()
endif

if maparg('<C-L>', 'n') ==# ''
  " From sensible.vim
  nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
endif

if 1
  function! s:LinewisePasteOp(type) abort
    let s_type = getregtype('s')
    if has('patch-7.4.513')
      let s_contents = getreg('s', 1, 1)
      let reg = getreg(v:register, 1, 1)
    else
      let s_contents = getreg('s')
      let reg = getreg(v:register)
    endif
    call setreg('s', reg, 'l')
    let l:do_after_paste = (s:post_paste ==# '' ? '' : s:post_paste . "']")
    exe 'normal! ' . v:count1 . '"s]' . s:paste_command . l:do_after_paste
    call setreg('s', s_contents, s_type)
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

if maparg(']<Space>', 'n') ==# ''
  function! s:BlankLinesOp(type) abort
    call append(line('.') + s:line_offset, repeat([''], v:count1))
  endfunction
  function! s:InsertBlankLinesWithOffset(line_offset) abort
    let s:line_offset = a:line_offset
    let &operatorfunc = matchstr(expand('<sfile>'), '<SNR>\d\+_') . 'BlankLinesOp'
    return 'g@l'
  endfunction
  nnoremap <expr> ]<Space> <SID>InsertBlankLinesWithOffset(0)
  nnoremap <expr> [<Space> <SID>InsertBlankLinesWithOffset(-1)
endif

if maparg(']q', 'n') ==# ''
  nnoremap <expr><silent> ]q ":<C-U>" . v:count1 . "cnext<CR>"
  nnoremap <expr><silent> [q ":<C-U>" . v:count1 . "cprevious<CR>"
endif

if exists('*strftime')
  " Modified from https://github.com/tpope/dotfiles/blob/c743f64380910041de605546149b0575ed0538ce/.vimrc#L284
  function! s:CompleteDateTime() abort
    let date_formats = ['%Y-%m-%d', '%B %-d, %Y', '%B %Y', '%Y-%m-%d %H:%M:%S', '%Y-%m-%d-at-%H-%M-%S']
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
  " See https://github.com/riceissa/computing-notes/blob/main/vim.md#why-no-c_comment_strings
  unlet! c_comment_strings
  " See https://github.com/riceissa/computing-notes/blob/main/vim.md#why-c_no_curly_error
  let c_no_curly_error = 1

  " See https://github.com/riceissa/computing-notes/blob/main/vim.md#python-indent
  let g:python_indent = {}
  let g:python_indent.open_paren = 'shiftwidth()'
  let g:python_indent.closed_paren_align_last_line = 0
  " Old style, for older versions of Vim; there is no equivalent of
  " closed_paren_align_last_line in the old style configuration.
  let g:pyindent_open_paren = '&sw'
endif

" See :help :DiffOrig
if exists(":DiffOrig") != 2
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_
        \ | diffthis | wincmd p | diffthis
endif

if !empty(globpath(&rtp, 'colors/vim.*'))
  colorscheme vim
  set notermguicolors
endif

if has('autocmd')
  augroup vimrc
    autocmd!
    autocmd InsertEnter * set listchars-=trail:@
    autocmd InsertLeave * set listchars+=trail:@
    if !has('nvim') && !has('patch-8.2.3464')
      autocmd BufNewFile,BufRead /etc/nginx/* setfiletype nginx
    endif
    " See https://github.com/riceissa/computing-notes/blob/main/vim.md#formatoptions
    autocmd FileType * set formatoptions-=o
    " See https://github.com/riceissa/computing-notes/blob/main/vim.md#haskell-shebang
    autocmd FileType haskell syntax match hsLineComment '^#!.*'
    autocmd FileType go,rust setlocal formatprg=
    autocmd FileType rust if &makeprg =~# '^rustc ' | setlocal makeprg=make | endif
    autocmd FileType rust setlocal textwidth=0
    " See https://github.com/riceissa/computing-notes/blob/main/vim.md#markdown-underscores
    autocmd FileType markdown setlocal spell iskeyword-=_
    autocmd FileType gitcommit setlocal spell
    autocmd FileType gitconfig setlocal commentstring=#\ %s
    autocmd FileType c,php,glsl setlocal commentstring=//\ %s
    " See https://github.com/riceissa/computing-notes/blob/main/vim.md#fix-gc-in-vim-files-in-neovim
    autocmd FileType vim setlocal textwidth=0 commentstring=\"\ %s
    autocmd FileType vim if &keywordprg ==# '' || &keywordprg ==# ':Man' | setlocal keywordprg=:help | endif
    if !(!empty(globpath(&rtp, 'plugin/editorconfig.*')) || (exists('+packpath') && !empty(globpath(&packpath, 'pack/*/opt/editorconfig'))))
      autocmd FileType vim setlocal expandtab shiftwidth=2 softtabstop=2
    endif
    autocmd FileType kitty setlocal commentstring=#\ %s
  augroup END

  " Remove the restore-cursor implementation on Fedora's /etc/vimrc, which used
  " to be broken. See https://bugzilla.redhat.com/show_bug.cgi?id=2404651 and
  " https://github.com/riceissa/computing-notes/blob/main/vim.md#vimrc-on-fedora
  " for more information.
  if exists('#fedora#BufReadPost *')
    autocmd! fedora BufReadPost *
  endif

  " See :help restore-cursor. This implementation is taken from Vim's
  " defaults.vim.
  augroup RestoreCursor
    autocmd!
    autocmd BufReadPost *
          \ let line = line("'\"")
          \ | if line >= 1 && line <= line("$") && &filetype !~# 'commit'
          \      && index(['xxd', 'gitrebase', 'tutor'], &filetype) == -1
          \      && !&diff
          \ |   execute "normal! g`\""
          \ | endif
  augroup END
endif
