set nocompatible
" Use vim-plug to manage Vim plugins. See https://github.com/junegunn/vim-plug
" for full instructions.
"
" Once all Vim config files are in the right places, just do :PlugInstall in
" Vim to install the plugins. The exception is YouCompleteMe, which probably
" needs to be compiled; see below for more.
call plug#begin('~/.vim/plugged')
Plug 'altercation/vim-colors-solarized' " only for gvim
Plug 'justinmk/vim-sneak'
Plug 'nelstrom/vim-visual-star-search'
Plug 'riceissa/vim-autolink'
Plug 'riceissa/vim-emacsctrll'
Plug 'riceissa/vim-markdown'
Plug 'riceissa/vim-markdown-paste'
Plug 'riceissa/vim-mediawiki'
Plug 'riceissa/vim-more-toggling'
Plug 'riceissa/vim-pdf-text-tools'
Plug 'tpope/vim-abolish'
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
" YouCompleteMe doesn't work with just :PlugInstall, so compile it as follows
" after calling :PlugInstall in Vim (which is still necessary since we have to
" clone the YouCompleteMe repository).
"    cd ~/.vim/plugged/YouCompleteMe
"    ./install.py --clang-completer
" Plug 'Valloric/YouCompleteMe', { 'for': ['python', 'java', 'c']}
call plug#end()

" Workaround for https://github.com/tpope/vim-sleuth/issues/29 to override
" sleuth.vim for some filetypes.
runtime! plugin/sleuth.vim

" Resolve disputes between `vim -Nu sensible.vim` and `nvim -u sensible.vim`
if &history < 10000
  set history=10000
endif
set nohlsearch
" From $VIMRUNTIME/vimrc_example.vim @ 97 if you have Vim 7.4.
" $VIMRUNTIME/defaults.vim @ 120 for Vim 8.0.
if has('langmap') && exists('+langnoremap')
  set langnoremap
endif
set listchars=tab:>\ ,trail:-,nbsp:+
if has('mouse')
  set mouse=a
endif
if has('path_extra')
  setglobal tags=./tags;,tags
endif

set modeline " Debian disables modeline
set number list ignorecase smartcase showcmd noequalalways nojoinspaces
set spellfile=~/.spell.en.add
set wildmode=list:longest,full

" From debian.vim
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc

" Make gq and gw accept a count in visual mode. So 72gq formats as if
" 'textwidth' is 72 regardless of what value it actually is. When no count is
" given, the commands work as usual.
vnoremap <silent> gq :<C-U>call <SID>GQ(v:count, 'gq')<CR>
vnoremap <silent> gw :<C-U>call <SID>GQ(v:count, 'gw')<CR>

function! s:GQ(tw, command)
  if v:count > 0
    let tmp = &tw
    let &tw = a:tw
    exe 'normal! gv' . a:command
    let &tw = tmp
  else
    exe 'normal! gv' . a:command
  endif
endfunction

nnoremap Y y$

" With man.vim loaded, <leader>K is more useful anyway
nnoremap K <C-^>
if !has('nvim')
  runtime! ftplugin/man.vim
endif
if has('nvim') && maparg('<Leader>K', 'n') ==# ''
  noremap <Leader>K :Man<CR>
endif

" First seen at http://vimcasts.org/episodes/the-edit-command/ but this
" particular version is from
" https://github.com/nelstrom/dotfiles/blob/448f710b855970a8565388c6665a96ddf4976f9f/vimrc#L80
cnoremap <expr> %% getcmdtype() == ':' ? fnameescape(expand('%:h')).'/' : '%%'

" HT Tim Pope https://github.com/tpope/tpope/blob/c743f64380910041de605546149b0575ed0538ce/.vimrc#L284
" I'm still not sure what the repeat(,0) is for...
if exists("*strftime")
  inoremap <silent> <C-G><C-T> <C-R>=repeat(complete(col('.'),map(["%F","%B %-d, %Y","%Y-%m-%d %H:%M:%S","%a, %d %b %Y %H:%M:%S %z","%Y %b %d","%d-%b-%y","%a %b %d %T %Z %Y"],'strftime(v:val)')+[localtime()]),0)<CR>
endif

" Quickly find characters that are not printable ASCII, which are sometimes
" undesirable to have in a file. This is best used along with
"     :setlocal nospell hlsearch syntax=OFF
" so that the characters in question stand out.
command! FindNonAscii /[^\d32-\d126]

" From defaults.vim; see also :help :DiffOrig
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
                  \ | wincmd p | diffthis
endif

if &t_Co >= 16
  " Changing ctermbg is useful for seeing tab with :set list
  highlight SpecialKey ctermfg=DarkGray ctermbg=LightGray
endif

let g:tex_flavor='latex'
if has('autocmd')
  augroup vimrc_au
    autocmd!
    " Seen at https://github.com/tpope/vim-sensible/issues/5 ; I think it's
    " better than resetting list or nolist by filetype, though one exception
    " is files that are meant to be read-only.
    autocmd InsertEnter * setlocal nolist
    autocmd InsertLeave * setlocal list
    autocmd FileType help,man setlocal nolist
    " Modified from :help ft-syntax-omni
    if exists("+omnifunc")
      autocmd FileType *
              \  if &omnifunc == "" |
              \    setlocal omnifunc=syntaxcomplete#Complete |
              \  endif
    endif
    autocmd FileType gitcommit,mail,markdown,mediawiki,tex setlocal spell
    autocmd FileType mail,text setlocal comments=fb:*,fb:-,fb:+,n:>
    autocmd FileType make setlocal noexpandtab
    autocmd FileType markdown,python setlocal expandtab shiftwidth=4
    autocmd FileType mediawiki noremap <buffer> j gj
    autocmd FileType mediawiki noremap <buffer> k gk
    autocmd FileType mediawiki noremap <buffer> gj j
    autocmd FileType mediawiki noremap <buffer> gk k
    " In Ubuntu 16.04, vim-gtk is compiled with python3 support but not python
    " support. However, the omnifunc check above tries to use
    " pythoncomplete#Complete here, which doesn't exist since there is no
    " python support. The solution is to force the python3 complete function.
    " (This is my guess as to what is going on, and will allow completion to
    " work, but I haven't investigated this issue in detail.)
    if has('python3')
      autocmd FileType python setlocal omnifunc=python3complete#Complete
    endif
    " Prevent overzealous autoindent in align environment
    autocmd FileType tex setlocal indentexpr=
    " More aggressively check spelling in LaTeX; see
    " http://stackoverflow.com/questions/5860154/vim-spell-checking-comments-only-in-latex-files
    autocmd FileType tex syntax spell toplevel
  augroup END
endif

if has('digraphs')
  " Horizontal ellipsis, …
  digraph el 8230
endif

let g:autolink_executable = '/home/issa/projects/autolink/autolink.py'
let g:autolink_download_provider = 'curl -L --silent --compressed'

let g:ycm_filetype_blacklist = {
    \ 'gitcommit': 1,
    \ 'html': 1,
    \ 'mail' : 1,
    \ 'markdown' : 1,
    \ 'mediawiki': 1,
    \ 'notes' : 1,
    \ 'pandoc' : 1,
    \ 'pdc' : 1,
    \ 'tex': 1,
    \ 'text': 1,
    \ 'unite' : 1,
    \}

let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'
let g:EclimCompletionMethod = 'omnifunc'
