set nocompatible
" Use vim-plug to manage Vim plugins. See
" <https://github.com/junegunn/vim-plug> for full instructions. Once all Vim
" config files are in the right places, just do :PlugInstall in Vim to install
" the plugins.
call plug#begin('~/.vim/plugged')
if has("gui_running")
  Plug 'romainl/flattened'
endif
Plug 'AndrewRadev/splitjoin.vim'
Plug 'fatih/vim-go'
Plug 'junegunn/fzf', {'dir': '~/.fzf', 'do': './install --all'}
Plug 'junegunn/gv.vim'
Plug 'lervag/vimtex', {'for': 'tex'}
" Plug 'ludovicchabant/vim-gutentags'
Plug 'nelstrom/vim-visual-star-search'
Plug 'riceissa/vim-cuaccp'
Plug 'riceissa/vim-dualist'
Plug 'riceissa/vim-inclusivespace'
Plug 'riceissa/vim-longmove'
Plug 'riceissa/vim-markdown'
Plug 'riceissa/vim-mediawiki'
Plug 'riceissa/vim-more-toggling'
Plug 'riceissa/vim-rsi'
Plug 'tpope/vim-characterize'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-rhubarb'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
call plug#end()

" Workaround for <https://github.com/tpope/vim-sleuth/issues/29> to override
" sleuth.vim for some filetypes.
runtime! plugin/sleuth.vim

" Override ttimeoutlen later
runtime! plugin/sensible.vim

" Resolve disputes between `vim -Nu sensible.vim` and `nvim -u sensible.vim`
if &history < 10000
  set history=10000
endif
set nohlsearch
if has('langmap') && exists('+langnoremap')
  set langnoremap
endif
if has('path_extra')
  setglobal tags=./tags;,tags
endif
if !has('nvim')
  set ttimeout
  set ttimeoutlen=50
endif

set nomodeline ignorecase smartcase showcmd noequalalways nojoinspaces
set spellfile=~/.spell.en.utf-8.add
set wildmode=list:longest,full
set sidescroll=1
if has('mouse')
  set mouse=nv
endif
if exists('&inccommand')
  set inccommand=split
endif

nnoremap Y y$
vnoremap K <nop>
if !has('nvim')
  runtime! ftplugin/man.vim
  nmap <silent> K <Leader>K
endif

" Quickly find potentially problematic characters (things like non-printing
" ASCII, exotic whitespace, and lookalike Unicode letters).
nnoremap g/ /[^\d32-\d126“”‘’–—§]<CR>

" First seen at <http://vimcasts.org/episodes/the-edit-command/> but this
" particular version is modified from
" <https://github.com/nelstrom/dotfiles/blob/448f710b855970a8565388c6665a96ddf4976f9f/vimrc#L80>
cnoremap %% <C-R><C-R>=getcmdtype() == ':' ? fnameescape(expand('%:h')).'/' : '%%'<CR>

" From Tim Pope, but I've unrolled it into multiple lines and changed some of
" the formats
" <https://github.com/tpope/tpope/blob/c743f64380910041de605546149b0575ed0538ce/.vimrc#L284>
if exists("*strftime")
  inoremap <C-G><C-T> <C-R>=<SID>ListDate()<CR>
  function! s:ListDate() abort
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
endif

if !exists(":DiffOrig")
  command DiffOrig call <SID>DiffOrig()
endif
function! s:DiffOrig()
  " Original DiffOrig; see :help :DiffOrig
  vert new | set bt=nofile | r ++edit # | 0d_ | diffthis | wincmd p | diffthis
  " Add a buffer-local mapping to the scratch buffer so that it is easier to
  " exit the diffing session
  wincmd p
  nnoremap <buffer><silent> q :diffoff!<Bar>quit<CR>
endfunction

if has('autocmd')
  augroup vimrc
    autocmd!
    autocmd BufNewFile,BufRead *.arbtt/categorize.cfg setlocal filetype=haskell
    autocmd BufNewFile,BufRead *.page setlocal filetype=markdown
    autocmd FileType crontab setlocal commentstring=#%s
    autocmd FileType gitcommit,mail,markdown,mediawiki,tex setlocal spell
    autocmd FileType mediawiki let b:surround_{char2nr('w')} = "[[wikipedia:\r|]]"
    autocmd FileType mediawiki let b:surround_{char2nr('r')} = "<ref name=\"\r\" />"
    autocmd FileType mediawiki setlocal linebreak
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
    autocmd FileType markdown setlocal expandtab shiftwidth=4 tabstop=4 textwidth=79
    " Allow opening of locally linked pages with gf
    autocmd BufNewFile,BufRead */issarice.com/wiki/*.md setlocal includeexpr=substitute(v:fname,'$','.md','')
    autocmd FileType mediawiki setlocal omnifunc=mediawikicomplete#Complete
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
    " <http://stackoverflow.com/questions/5860154/vim-spell-checking-comments-only-in-latex-files>
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

iabbrev ADd Add
iabbrev REmove Remove

let g:tex_flavor='latex'
let g:surround_{char2nr('q')} = "“\r”"
let g:surround_{char2nr('Q')} = "‘\r’"
let g:dualist_color_listchars = 1

if has("clipboard")
  let g:cuaccp_no_mappings = 1
  vmap x <Plug>CuaccpVCut
  vmap <C-C> <Plug>CuaccpVCopy
  nmap <C-V> <Plug>CuaccpNPaste
  cmap <C-V> <Plug>CuaccpCPaste
  imap <C-V> <Plug>CuaccpIPaste
  vmap <C-V> <Plug>CuaccpVPaste
  imap <C-G><C-V> <Plug>CuaccpIHardwrapPaste
endif

if has("gui_running")
  silent! colorscheme flattened_light
  set guioptions-=m
  set guioptions-=T
else
  highlight Visual ctermfg=White ctermbg=Gray
  highlight Folded ctermfg=DarkGray ctermbg=LightGray cterm=bold,underline
endif
