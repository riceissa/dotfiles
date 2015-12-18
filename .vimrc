set nocompatible
set nomodeline modelines=0
filetype plugin indent on
syntax enable
inoremap <C-u> <C-g>u<C-u>
inoremap <C-w> <C-g>u<C-w>
inoremap <C-r> <C-g>u<C-r>
nnoremap K <C-^>
set hidden number ruler showcmd
set expandtab shiftwidth=4 softtabstop=4 tabstop=4
set spellfile=~/.spell.en.add
set wildmode=list:longest,full

if $TERM ==? "xterm-256color" || $TERM ==? "screen-256color"
    set t_Co=256
    highlight SpecialKey ctermfg=DarkGray ctermbg=LightGray
endif

if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
    runtime! macros/matchit.vim
endif

" Explicitly set options that are changed by Neovim, for compatibility.  This
" allows a single .vimrc file to be used for both Vim and Neovim.
set autoindent
set noautoread
set backspace=indent,eol,start
set complete-=i
set display=lastline
if &encoding !=? 'utf-8'
    set encoding=utf-8
endif
set formatoptions=tcqj
set history=10000
set nohlsearch
set incsearch
if exists('+langnoremap')
    set langnoremap
endif
set laststatus=1
set listchars=tab:>\ ,trail:@,nbsp:_
set mouse=a " Always enable mouse
set nrformats=hex
set sessionoptions-=options
set smarttab
set tabpagemax=50
setglobal tags=./tags;,tags
set ttyfast
set viminfo^=!
set wildmenu

let g:tex_flavor='latex'
augroup filetype_specific
    autocmd!
    autocmd filetype gitcommit setlocal spell
    autocmd filetype html,xhtml,xml setlocal shiftwidth=2 softtabstop=2 tabstop=2
    autocmd filetype tex setlocal indentexpr=
    autocmd filetype mail setlocal linebreak spell
    autocmd filetype make setlocal noexpandtab
    autocmd BufNewFile,BufRead *.md,*.page,*.pdc setlocal filetype=markdown
    autocmd filetype markdown setlocal linebreak spell
augroup END
