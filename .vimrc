set nocompatible
" Use vim-plug to manage Vim plugins. See https://github.com/junegunn/vim-plug
" for full instructions. Once all Vim config files are in the right places,
" just do :PlugInstall in Vim to install the plugins. The exception is
" YouCompleteMe, which needs to be compiled; see below for more.
call plug#begin('~/.vim/plugged')
if has("gui_running")
  Plug 'altercation/vim-colors-solarized' " Only for gvim
endif
Plug 'AndrewRadev/splitjoin.vim'
Plug 'fatih/vim-go'
Plug 'junegunn/fzf', {'dir': '~/.fzf', 'do': './install --all'}
Plug 'junegunn/gv.vim'
Plug 'lervag/vimtex', {'for': 'tex'}
" Plug 'ludovicchabant/vim-gutentags'
Plug 'majutsushi/tagbar'
Plug 'nelstrom/vim-visual-star-search'
Plug 'riceissa/vim-cuaccp'
Plug 'riceissa/vim-longmove'
Plug 'riceissa/vim-markdown'
Plug 'riceissa/vim-mediawiki'
Plug 'riceissa/vim-more-toggling'
Plug 'riceissa/vim-rsi'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-characterize'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rhubarb'
if !has('nvim')
  Plug 'tpope/vim-sensible'
endif
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
call plug#end()

" Workaround for https://github.com/tpope/vim-sleuth/issues/29 to override
" sleuth.vim for some filetypes.
runtime! plugin/sleuth.vim

" Override ttimeoutlen later
runtime! plugin/sensible.vim

" Resolve disputes between `vim -Nu sensible.vim` and `nvim -u NORC`
if &history < 10000
  set history=10000
endif
set nohlsearch
if has('langmap') && exists('+langnoremap')
  set langnoremap
endif
set listchars=tab:▸\ ,trail:·,nbsp:+
if has('mouse')
  set mouse=a
endif
set ruler
set scrolloff=1
set sidescrolloff=5
if has('path_extra')
  setglobal tags=./tags;,tags
endif
if !has('nvim')
  set ttimeout
  set ttimeoutlen=50
endif
nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
inoremap <C-U> <C-G>u<C-U>

set modeline " Debian disables modeline
set number list ignorecase smartcase showcmd noequalalways nojoinspaces
set spellfile=~/.spell.en.utf-8.add
set wildmode=list:longest,full
set sidescroll=1

" From debian.vim
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc

nnoremap Y y$

if !has('nvim')
  runtime! ftplugin/man.vim
endif
if has('nvim') && maparg('<Leader>K', 'n') ==# ''
  noremap <Leader>K :Man<CR>
endif

" Quickly find potentially problematic characters (things like non-printing
" ASCII, exotic whitespace, and lookalike Unicode letters). This may be
" combined with something like
"     :setlocal nospell hlsearch syntax=OFF
" so that the characters in question stand out.
nnoremap g/ /[^\d32-\d126“”‘’–—§]<CR>

" First seen at http://vimcasts.org/episodes/the-edit-command/ but this
" particular version is modified from
" https://github.com/nelstrom/dotfiles/blob/448f710b855970a8565388c6665a96ddf4976f9f/vimrc#L80
cnoremap %% <C-R><C-R>=getcmdtype() == ':' ? fnameescape(expand('%:h')).'/' : '%%'<CR>

" From Tim Pope, but I've unrolled it into multiple lines
" <https://github.com/tpope/tpope/blob/c743f64380910041de605546149b0575ed0538ce/.vimrc#L284>
inoremap <C-G><C-T> <C-R>=<SID>ListDate()<CR>
function! s:ListDate()
    let date_fmts = [
          \ "%F",
          \ "%B %-d, %Y",
          \ "%B %Y",
          \ "%F %a",
          \ "%F %a %H:%M",
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

" From Tim Pope:
" <https://github.com/tpope/tpope/blob/c743f64380910041de605546149b0575ed0538ce/.vimrc#L271>
nmap <silent> s :if &previewwindow<Bar>pclose<Bar>elseif exists(':Gstatus')<Bar>exe 'Gstatus'<Bar>else<Bar>ls<Bar>endif<CR>

nnoremap <silent> S :if exists(':Git')<Bar>update<Bar>exe 'silent !clear'<Bar>exe 'Git diff ' . shellescape(expand("%:p"))<Bar>else<Bar>exe 'DiffOrig'<Bar>endif<CR>

nnoremap <silent> K :if exists(':Gwrite')<Bar>exe 'Gwrite'<Bar>exe 'Gcommit'<Bar>else<Bar>write<Bar>endif<CR>

if !exists(":DiffOrig")
  command DiffOrig call DiffOrig()
endif
function! DiffOrig()
  " Original DiffOrig; see :help :DiffOrig
  vert new | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis
  " Add a buffer-local mapping to the scratch buffer so that it is easier to
  " exit the diffing session
  wincmd p
  nnoremap <buffer><silent> q :diffoff!<Bar>quit<CR>
endfunction

if exists('&inccommand')
  set inccommand=split
endif

function! s:ColorListChars()
  if &t_Co >= 16
    " Changing ctermbg is useful for seeing tab with :set list
    if &background ==# "dark"
      highlight SpecialKey ctermfg=LightGray ctermbg=DarkGray
      if has('nvim')
        highlight Whitespace ctermfg=LightGray ctermbg=DarkGray
      endif
    else
      highlight SpecialKey ctermfg=DarkGray ctermbg=LightGray
      if has('nvim')
        highlight Whitespace ctermfg=DarkGray ctermbg=LightGray
      endif
    endif
  endif
endfunction
call <SID>ColorListChars()

let g:tex_flavor='latex'
if has('autocmd')
  augroup my_init
    autocmd!
    autocmd BufNewFile,BufRead *.arbtt/categorize.cfg setlocal filetype=haskell
    autocmd BufNewFile,BufRead *.page setlocal filetype=markdown
    autocmd FileType crontab setlocal commentstring=#%s
    autocmd FileType gitcommit,mail,markdown,mediawiki,tex setlocal spell
    autocmd InsertEnter * set listchars=tab:▸\ ,nbsp:+
    autocmd InsertLeave * set listchars=tab:▸\ ,trail:·,nbsp:+
    if exists('##OptionSet')
      autocmd OptionSet background call <SID>ColorListChars()
    endif
    autocmd FileType help,man setlocal nolist nospell
    autocmd FileType help,man nnoremap <buffer> <silent> q :q<CR>
    " Modified from :help ft-syntax-omni
    if exists("+omnifunc")
      autocmd FileType *
              \  if &omnifunc == "" |
              \    setlocal omnifunc=syntaxcomplete#Complete |
              \  endif
    endif
    autocmd FileType mail,text,help setlocal comments=fb:*,fb:-,fb:+,n:>
    autocmd FileType make setlocal noexpandtab
    " sleuth.vim usually detects 'shiftwidth' as 2, though this depends on how
    " the Markdown is written.
    autocmd FileType markdown setlocal expandtab shiftwidth=4 textwidth=79
    " In some versions, when Vim is compiled with python3 support but not
    " python support, the omnifunc check above tries to use
    " pythoncomplete#Complete, which doesn't exist since there is no python
    " support. The solution is to force the python3 complete function.
    if has('python3')
      autocmd FileType python setlocal omnifunc=python3complete#Complete
    endif
    " Prevent overzealous autoindent in align environment
    autocmd FileType tex setlocal indentexpr=
    autocmd FileType tex let b:surround_{char2nr('m')} = "\\(\r\\)"
    autocmd FileType tex let b:surround_{char2nr('M')} = "\\[\n\r\n\\]"
    " More aggressively check spelling in LaTeX; see
    " http://stackoverflow.com/questions/5860154/vim-spell-checking-comments-only-in-latex-files
    autocmd FileType tex syntax spell toplevel
    " From defaults.vim
    " When editing a file, always jump to the last known cursor position.
    " Don't do it when the position is invalid or when inside an event handler
    " (happens when dropping a file on gvim).
    autocmd BufReadPost *
      \ if line("'\"") >= 1 && line("'\"") <= line("$") |
      \   exe "normal! g`\"" |
      \ endif
  augroup END
endif

" Fix common typos where one character is stuck to the beginning of the next
" word or the end of the last word.
inoremap <C-G>h <C-G>u<Esc>BxgEpgi
inoremap <C-G>l <C-G>u<Esc>gExpgi

" Now that I have a unified way to search for common Unicode characters
" (unicode.vim and my own completefunc), I'm not sure I'll need this digraph
" anymore. Keeping it in experimental; if I don't use it for a while I'll just
" remove it in favor of completefunc.
if has('digraphs')
  " Horizontal ellipsis, …
  digraph el 8230
  digraph ./ 8230
  digraph ^\| 8593
  digraph \\ 8726
  digraph \- 8726
  digraph -\ 8726
  digraph \|> 8614
  digraph v\| 8595
  " Run under exe so that syntax highlighting isn't messed up
  exe 'digraph (/ 8713'
  exe 'digraph (\ 8713'
  exe 'digraph (< 10216'
  exe 'digraph >) 10217'
endif

" This map makes it easier to search across linebreaks. If you want to
" search for "hello there" but there might be a linebreak in between
" the "hello" and the "there", you will have to do something like
" "hello[ \n]there". But what about other whitespace? Okay, you say, I'll
" search for "hello\_s\+there" to catch one or more whitespace characters,
" including newlines. But what if the phrase occurs in a comment? In that case
" there might be a comment character in front of the "there". And what if this
" is a Markdown document and the phrase occurs inside a blockquote? You don't
" *really* want to search for
" "hello\(\_s\+\|\V<!--\m\|\V-->\m\|\V>\m\)\+there" each time, do you?
" Instead, with this map, just type "hello" then <C-X><Space> then "there".
cnoremap <expr> <C-X><Space> "<C-R>=<SID>InclusiveSpace('" . getcmdtype() . "')<CR>"
function! s:InclusiveSpace(cmdtype)
  " TODO also get 'm' (and others?) from &comments.
  let l:result = '\(\_s\+'
  if &commentstring !=# ""
    " Try to get the parts of the commentstring, e.g. "<!--" and "-->" for
    " HTML, "/*" and "*/" for C.
    for l:cmt in split(&commentstring, '%s')
      if l:cmt !=# ""
        " Strip whitespace
        let l:cmt = substitute(l:cmt, '^\s*\(.\{-}\)\s*$', '\1', '')
        let l:result .= '\|\V' . escape(l:cmt, a:cmdtype.'\') . '\m'
      endif
    endfor
  endif
  if &comments !=# ""
    for l:cmt in split(&comments, ',')
      if l:cmt =~# "n:"
        " Strip the "n:"
        let l:cmt = strpart(l:cmt, 2)
        " Strip whitespace
        let l:cmt = substitute(l:cmt, '^\s*\(.\{-}\)\s*$', '\1', '')
        let l:result .= '\|\V' . escape(l:cmt, a:cmdtype.'\') . '\m'
      endif
    endfor
  endif
  let l:result .= '\)\+'
  return l:result
endfunction

inoremap <C-G><C-W> <C-\><C-O>"-dB
inoremap <C-G><C-K> <C-\><C-O>"-D
cnoremap <C-X><C-K> <C-\>eCmdlineKillToEnd()<CR>
function! CmdlineKillToEnd()
  let pos = getcmdpos()
  if pos == 1
    " Vim's string indexing is messed up so I think we need a special case
    " here. cmd[0 : -1] would select the whole string.
    return ""
  else
    let cmd = getcmdline()
    " subtract two because right index is inclusive and because getcmdpos()
    " starts at 1
    return cmd[0 : pos-2]
  endif
endfunction
cnoremap <C-X><C-F> <C-F>
cnoremap <C-X><C-D> <C-D>
inoremap <C-G><C-D> <C-\><C-O>"-dE
" Reverse the effects of 'textwidth' in insert mode. This is useful if most of
" the lines in a file have one textwidth but a couple have a different one
" (examples: a Markdown file that has a textwidth of 79 but where link
" references have unlimited line length so that they can be sorted; a Vim
" script file that has a textwidth of 78 but where you want one particular
" line to be just a little longer; a Python file that has a textwidth of 0 but
" where you want to format docstrings as you write them). When 'textwidth' is
" disabled, it will format the current line; otherwise it will join back the
" current line with the previous one. The latter works well with :setl fo+=l
" to prevent repeatedly breaking the line.
inoremap <expr> <C-G><C-G> (&textwidth == 0) ? '<C-\><C-O>gww' : '<Esc>kJgi'

function! s:BrowseNewTab(progname)
  tabnew
  set bt=nofile
  setl nonumber
  exec "0r !fetch-page " . a:progname
  let b:url = @+
  1
  " If the buffer is the output of lynx -dump, then with the cursor on a
  " number, the following will jump to the link reference with that number.
  nnoremap <buffer><expr> <C-]> 'm'':$?^ \+' . expand("<cword>") . '\.?<CR>W'
endfunction
command! BrowseNewTab :call <SID>BrowseNewTab("wget")
command! BrowseNewTabCurl :call <SID>BrowseNewTab("curl")

" These use an external program available at
" https://github.com/riceissa/pdftextfmt
nnoremap <silent> gQ Vip:!pdftextfmt<CR>:<C-R>=&textwidth>0?'normal! gqq':''<CR><CR>
vnoremap <silent> gQ :!pdftextfmt<CR>:<C-R>=&textwidth>0?'normal! gqq':''<CR><CR>

vnoremap K <nop>
cnoremap <C-O> <Up>

iabbrev ADd Add
iabbrev REmove Remove

let g:surround_{char2nr('q')} = "“\r”"
let g:surround_{char2nr('Q')} = "‘\r’"

let g:cuaccp_no_mappings = 1
vmap x <Plug>CuaccpVCut
vmap <C-C> <Plug>CuaccpVCopy
nmap <C-V> <Plug>CuaccpNPaste
cmap <C-V> <Plug>CuaccpCPaste
imap <C-V> <Plug>CuaccpIPaste
vmap <C-V> <Plug>CuaccpVPaste
imap <C-G><C-V> <Plug>CuaccpIHardwrapPaste
