set nocompatible
if filereadable(expand('~/.vim/plugins.vim'))
    source ~/.vim/plugins.vim
endif
filetype plugin indent on
set nomodeline modelines=0
syntax enable

inoremap <C-R> <C-G>u<C-R>

" Mappings that conflict with the muscle memory from using default Vim
inoremap <C-L> <C-G>u<C-O>zz
" With man.vim loaded, <leader>K is more useful anyway
nnoremap K <C-^>
nnoremap Y y$
" Condensed version of the characterwise insert mode mapping from
" $VIMRUNTIME/autoload/nocompatiblexpaste.vim
inoremap <C-R>+ <C-G>ux<Esc>"=@+.'xy'<CR>gPFx"_2x"_s

let mapleader = ' '
nnoremap gh F<Space>xpA
nnoremap gH F<Space>gExpA
nnoremap <leader>m :update \| !make<CR><CR>
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

" If `vim -Nu sensible.vim` and `nvim -u sensible.vim` disagree on an option,
" this file will resolve the dispute.
if &history < 10000
  set history=10000
endif
set nohlsearch
if has('langmap') && exists('+langnoremap')
  set langnoremap
endif
set listchars=tab:>\ ,trail:-,nbsp:+
set mouse=a
if has('path_extra')
  setglobal tags=./tags;,tags
endif

if $TERM ==? "xterm-256color" || $TERM ==? "screen-256color"
    set t_Co=256
    highlight SpecialKey ctermfg=DarkGray ctermbg=LightGray
    highlight StatusLine ctermbg=LightGray ctermfg=DarkGray cterm=none
endif

" See :help ft-syntax-omni
if has("autocmd") && exists("+omnifunc")
    autocmd Filetype *
            \    if &omnifunc == "" |
            \        setlocal omnifunc=syntaxcomplete#Complete |
            \    endif
endif

" Use with <C-r>=Today<tab><CR>
" This is useful when I don't have UltiSnips
function! Today()
    return strftime("%Y-%m-%d")
endfunction

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
    augroup filetype_specific
        autocmd!
        " Seen at https://github.com/tpope/vim-sensible/issues/5 ; I think
        " it's better than resetting list or nolist by filetype.
        autocmd InsertEnter * setlocal nolist
        autocmd InsertLeave * setlocal list
        autocmd BufNewFile,BufRead *.md,*.page,*.pdc setlocal filetype=markdown
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
        autocmd FileType html,xhtml,xml setlocal shiftwidth=2 softtabstop=2 tabstop=2
        autocmd FileType mail setlocal linebreak spell
        autocmd FileType make setlocal noexpandtab
        autocmd FileType markdown setlocal syntax=
        autocmd FileType markdown setlocal linebreak spell textwidth=80
        autocmd FileType gitcommit,markdown,mail,mediawiki,tex setlocal spell
        autocmd FileType mediawiki noremap <buffer> j gj
        autocmd FileType mediawiki noremap <buffer> k gk
        autocmd FileType mediawiki noremap <buffer> gj j
        autocmd FileType mediawiki noremap <buffer> gk k
        "autocmd FileType markdown setlocal nonumber showbreak=\\
        " modified from $VIM/vim74/syntax/mail.vim
        autocmd FileType markdown syn match markdownURL contains=@NoSpell `\v<(((https?|ftp|gopher)://|(mailto|file|news):)[^' 	<>")]+|(www|web|w3)[a-zA-Z0-9_-]*\.[a-zA-Z0-9._-]+\.[^' 	<>")]+)[a-zA-Z0-9/]`
        hi def link markdownURL String
        " Prevent overzealous autoindent in align environment
        autocmd FileType tex setlocal indentexpr=
        autocmd FileType tex setlocal spell
        autocmd FileType tex syntax spell toplevel
    augroup END
endif

" Ellipsis, …
digraph el 8230
" Left and right angle brackets, ⟨ ⟩
digraph (< 10216
digraph <( 10216
digraph )> 10217
digraph >) 10217
