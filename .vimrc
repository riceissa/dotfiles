" Use vim-plug to manage Vim plugins. See https://github.com/junegunn/vim-plug
" for full instructions.
"
" Once all Vim config files are in the right places, just do :PlugInstall in
" Vim to install the plugins. The exception is YouCompleteMe, which probably
" needs to be compiled; see below for more.
call plug#begin('~/.vim/plugged')
Plug 'derekwyatt/vim-scala'
Plug 'altercation/vim-colors-solarized' " only for gvim
Plug 'justinmk/vim-sneak'
Plug 'nelstrom/vim-visual-star-search'
Plug 'riceissa/vim-autolink'
Plug 'riceissa/vim-easy-quoteplus'
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
" Plug 'SirVer/ultisnips'
" Plug 'ctrlpvim/ctrlp.vim'
" Plug 'honza/vim-snippets'
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
" From $VIMRUNTIME/vimrc_example.vim @ 97
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
set notimeout ttimeout
set list ignorecase smartcase showcmd noequalalways nojoinspaces
set spellfile=~/.spell.en.add
set wildmode=list:longest,full

inoremap <C-R> <C-G>u<C-R>
" HT Emacs for the idea; I think this algorithm (which is similar to but not
" the same as what Emacs uses) works for *most* files but I can't promise
" anything.
function! EmacsCtrlL()
  if abs(winline()) <= 1+&scrolloff
    echom "case 1"
    return 'zb'
  elseif abs(winline() - winheight(0)/2) <= 2
    echom "case 2"
    return 'zt'
  elseif abs(winline() - winheight(0)) <= 1+&scrolloff
    echom "case 3"
    return 'zz'
  else
    echom "case 4"
    return 'zz'
  endif
endfunction
"inoremap <expr> <C-L> '<C-\><C-O>' . ':setl nowrap<CR><C-\><C-O>' . ':let p = EmacsCtrlL()<CR>' . '<C-\><C-O>:setl wrap<CR><C-\><C-O>' . p
inoremap <expr> <C-L> (pumvisible() <bar><bar> &insertmode) ? '<C-L>' : '<C-\><C-O>' . EmacsCtrlL()
" With man.vim loaded, <leader>K is more useful anyway
nnoremap K <C-^>
nnoremap Y y$

" Condensed version of the characterwise insert mode mapping from
" $VIMRUNTIME/autoload/paste.vim. I notice a slight lag with this mapping
" compared to riceissa/safe-paste.vim. I suspect this is because this mapping
" has to type in the extra 'x' and 'xy' into the buffer only to delete them
" again (somehow causing the momentary lag), whereas safe-paste.vim does all
" the calculation "away from the buffer", and only pastes when the positioning
" has been done. The 'xy' is what forces pasting to be characterwise, because
" it causes the expression not to end in a newline. One more problem: this
" mapping clears out the ". register; the default mapping keeps the pasted
" text in it. See
" http://vim.wikia.com/wiki/Pasting_registers?oldid=39352?useskin=monobook#In_insert_and_command-line_modes
if has('clipboard')
  inoremap <C-R>+ <C-G>ux<Esc>"=@+.'xy'<CR>gPFx"_2x"_s
endif

" Quickly fix two forms of typo I often make. The default gh and gH, used to
" enter select mode, are only useful in mappings anyway. To quote Drew Neil,
" "If you are happy to embrace the modal nature of Vim, then you should find
" little use for Select mode, which holds the hand of users who want to make
" Vim behave more like other text editors." (Practical Vim, pg 41)
nnoremap gh F<Space>xpA
nnoremap gH F<Space>gExpA

" First seen at http://vimcasts.org/episodes/the-edit-command/ , but this
" particular version is from
" https://github.com/nelstrom/dotfiles/blob/448f710b855970a8565388c6665a96ddf4976f9f/vimrc#L80
cnoremap <expr> %% getcmdtype() == ':' ? fnameescape(expand('%:h')).'/' : '%%'

" HT Tim Pope https://github.com/tpope/tpope/blob/c743f64380910041de605546149b0575ed0538ce/.vimrc#L284
" I'm still not sure what the repeat(,0) is for...
if exists("*strftime")
  inoremap <silent> <C-G><C-T> <C-R>=repeat(complete(col('.'), map([
    \ "%F",
    \ "%B %-d, %Y",
    \ "%Y-%m-%d %H:%M:%S",
    \ "%a, %d %b %Y %H:%M:%S %z",
    \ "%Y %b %d",
    \ "%d-%b-%y",
    \ "%a %b %d %T %Z %Y"
  \ ], 'strftime(v:val)') + [
    \ localtime()
  \ ]), 0)<CR>
endif

" Quickly find characters that are not printable ASCII, which are sometimes
" undesirable to have in a file. This is best used along with
"     :setlocal nospell hlsearch syntax=OFF
" so that the characters in question stand out.
command! FindNonAscii /[^\d32-\d126]

if &t_Co >= 16
  " Changing ctermbg is useful for seeing tab with :set list
  highlight SpecialKey ctermfg=DarkGray ctermbg=LightGray
endif

if !has('nvim')
  runtime! ftplugin/man.vim
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

let g:UltiSnipsSnippetDirectories=["UltiSnips", "UltiSnips-custom-snippets"]
let g:UltiSnipsExpandTrigger="<C-j>"
let g:UltiSnipsJumpForwardTrigger="<C-j>"
let g:UltiSnipsJumpBackwardTrigger="<C-k>"
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

let g:pandoc#syntax#conceal#use = 0

" Ignore files in .gitignore
let g:ctrlp_user_command = ['.git/',
  \ 'git --git-dir=%s/.git ls-files -oc --exclude-standard']
let g:ctrlp_map = '<c-k>'
