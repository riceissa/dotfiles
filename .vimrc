call plug#begin('~/.vim/plugged')
Plug 'justinmk/vim-sneak'
Plug 'nelstrom/vim-visual-star-search'
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

inoremap <C-R> <C-G>u<C-R>
" With man.vim loaded, <leader>K is more useful anyway
nnoremap K <C-^>
nnoremap Y y$
" Condensed version of the characterwise insert mode mapping from
" $VIMRUNTIME/autoload/nocompatiblexpaste.vim
inoremap <C-R>+ <C-G>ux<Esc>"=@+.'xy'<CR>gPFx"_2x"_s

nnoremap gh F<Space>xpA
nnoremap gH F<Space>gExpA
" First seen at http://vimcasts.org/episodes/the-edit-command/ , but this
" particular version is from
" https://github.com/nelstrom/dotfiles/blob/448f710b855970a8565388c6665a96ddf4976f9f/vimrc#L80
cnoremap <expr> %% getcmdtype() == ':' ? fnameescape(expand('%:h')).'/' : '%%'

set hidden number showcmd noequalalways nojoinspaces
set expandtab shiftwidth=4 softtabstop=4 tabstop=4
set matchpairs+=<:>,“:”,«:»
set spellfile=~/.spell.en.add
set wildmode=list:longest,full
set ignorecase smartcase
if &encoding !=? 'utf-8'
    set encoding=utf-8
endif

if &t_Co >= 16
  " Changing ctermbg is useful for seeing tab with :set list
  highlight SpecialKey ctermfg=DarkGray ctermbg=LightGray
endif

" See :help ft-syntax-omni
if has("autocmd") && exists("+omnifunc")
    autocmd Filetype *
            \    if &omnifunc == "" |
            \        setlocal omnifunc=syntaxcomplete#Complete |
            \    endif
endif

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

if executable('autolink.py') && has('clipboard')
    " See https://github.com/riceissa/autolink for source
    function! PasteLink(fmt)
        " Escape double and single quotes and backslashes to prevent
        " potential attacks against oneself
        let link = substitute(@+, '"', '%22', 'g')
        let link = substitute(link, "'", "%27", "g")
        let link = substitute(link, '\', "%5C", "g")
        if a:fmt ==? ''
            let command = "autolink.py --clean --format none '" . link . "'"
        else
            let command = "autolink.py --clean --format " . a:fmt . " '" . link . "'"
        endif
        return system(command)
    endfunction
    " Break up the undo first in case the output is messed up
    " Note that this map also works with Ctrl-/
    inoremap <C-_> <C-G>u<C-r>=PasteLink(&filetype)<CR>
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
        autocmd FileType gitcommit setlocal spell
        " In Ubuntu 16.04, vim-gtk is compiled with python3 support but not
        " python support. However, the omnifunc check above tries to use
        " pythoncomplete#Complete here, which doesn't exist since there is no
        " python support. The solution is to force the python3 complete
        " function. (This is my guess as to what is going on, and will allow
        " completion to work, but I haven't investigated this issue in
        " detail.)
        if has('python3')
            autocmd FileType python setlocal omnifunc=python3complete#Complete
        endif
        autocmd FileType make setlocal noexpandtab
        autocmd FileType markdown setlocal linebreak spell textwidth=80
        autocmd FileType gitcommit,markdown,mail,mediawiki,tex setlocal spell
        autocmd FileType mediawiki noremap <buffer> j gj
        autocmd FileType mediawiki noremap <buffer> k gk
        autocmd FileType mediawiki noremap <buffer> gj j
        autocmd FileType mediawiki noremap <buffer> gk k
        " Prevent overzealous autoindent in align environment
        autocmd FileType tex setlocal indentexpr=
        autocmd FileType tex setlocal spell
        autocmd FileType tex syntax spell toplevel
    augroup END
endif

if has('digraphs')
  " Horizontal ellipsis, …
  digraph el 8230
endif
