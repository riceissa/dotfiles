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
Plug 'justinmk/vim-sneak'
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
Plug 'tpope/vim-sensible'
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

" Resolve disputes between `vim -Nu sensible.vim` and `nvim -u sensible.vim`
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
if has('path_extra')
  setglobal tags=./tags;,tags
endif

set modeline " Debian disables modeline
set number list ignorecase smartcase showcmd noequalalways nojoinspaces
set spellfile=~/.spell.en.utf-8.add
set wildmode=list:longest,full
set ttimeoutlen=50 sidescroll=1

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

if exists('&inccommand')
  set inccommand=split
endif

" From defaults.vim; see also :help :DiffOrig
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
                  \ | wincmd p | diffthis
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
    autocmd OptionSet background call <SID>ColorListChars()
    autocmd FileType help,man setlocal nolist nospell
    autocmd FileType help nnoremap <buffer> q :q<CR>
    " Modified from :help ft-syntax-omni
    if exists("+omnifunc")
      autocmd FileType *
              \  if &omnifunc == "" |
              \    setlocal omnifunc=syntaxcomplete#Complete |
              \  endif
    endif
    autocmd FileType mail,text setlocal comments=fb:*,fb:-,fb:+,n:>
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
let g:ycm_python_binary_path = '/usr/bin/python3'
let g:EclimCompletionMethod = 'omnifunc'

let g:jedi#documentation_command = "<leader>K"

if filereadable(expand('~/.vim/experimental.vim'))
  source ~/.vim/experimental.vim
endif
