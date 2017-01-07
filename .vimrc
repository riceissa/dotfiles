set nocompatible
" Use vim-plug to manage Vim plugins. See https://github.com/junegunn/vim-plug
" for full instructions. Once all Vim config files are in the right places,
" just do :PlugInstall in Vim to install the plugins. The exception is
" YouCompleteMe, which needs to be compiled; see below for more.
call plug#begin('~/.vim/plugged')
Plug 'altercation/vim-colors-solarized' " Only for gvim
Plug 'chrisbra/unicode.vim'
Plug 'fatih/vim-go'
Plug 'junegunn/fzf', {'dir': '~/.fzf', 'do': './install --all'}
Plug 'junegunn/gv.vim'
Plug 'lervag/vimtex', {'for': 'tex'}
" Plug 'ludovicchabant/vim-gutentags'
Plug 'majutsushi/tagbar'
Plug 'nelstrom/vim-visual-star-search'
Plug 'riceissa/vim-autolink'
Plug 'riceissa/vim-emacsctrll'
Plug 'riceissa/vim-longmove'
Plug 'riceissa/vim-markdown'
Plug 'riceissa/vim-markdown-paste'
Plug 'riceissa/vim-mediawiki'
Plug 'riceissa/vim-more-toggling'
Plug 'riceissa/vim-rsi'
Plug 'tpope/vim-dispatch'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-characterize'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
if !has('nvim')
  Plug 'tpope/vim-sensible'
endif
Plug 'tpope/vim-sleuth'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'
" Plug 'davidhalter/jedi-vim', {'for': ['python']}
" YouCompleteMe doesn't work with just :PlugInstall, so compile it as follows
" after calling :PlugInstall in Vim (which is still necessary since we have to
" clone various repositories).
"     cd ~/.vim/plugged/YouCompleteMe
"     ./install.py --clang-completer
" Plug 'Valloric/YouCompleteMe', {'for': ['python']}
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
" From $VIMRUNTIME/vimrc_example.vim @ 97 if you have Vim 7.4.
" See
"   :edit +/langmap $VIMRUNTIME/defaults.vim
" for Vim 8.0.
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

" With man.vim loaded, <leader>K is more useful anyway
nnoremap K <C-^>
if !has('nvim')
  runtime! ftplugin/man.vim
endif
if has('nvim') && maparg('<Leader>K', 'n') ==# ''
  noremap <Leader>K :Man<CR>
endif

if exists('&inccommand')
  set inccommand=split
endif

function! s:ColorListChars()
  if &t_Co >= 16
    " Changing ctermbg is useful for seeing tab with :set list
    if &background ==# "dark"
      highlight SpecialKey ctermfg=LightGray ctermbg=DarkGray
    else
      highlight SpecialKey ctermfg=DarkGray ctermbg=LightGray
    endif
  endif
endfunction
call <SID>ColorListChars()

let g:tex_flavor='latex'
if has('autocmd')
  augroup my_init
    autocmd!
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
    autocmd FileType markdown setlocal expandtab shiftwidth=4
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
    autocmd FileType tex let b:surround_{char2nr('m')} = "\\(\r\\)"
    autocmd FileType tex let b:surround_{char2nr('M')} = "\\[\n\r\n\\]"
    " More aggressively check spelling in LaTeX; see
    " http://stackoverflow.com/questions/5860154/vim-spell-checking-comments-only-in-latex-files
    autocmd FileType tex syntax spell toplevel
  augroup END
endif

let g:autolink_executable = '/home/issa/projects/autolink/autolink.py'
let g:autolink_download_provider = 'curl -L --silent --compressed'

if filereadable(expand('~/.vim/experimental.vim'))
  source ~/.vim/experimental.vim
endif
