" This is free and unencumbered software released into the public
" domain.
"
" Anyone is free to copy, modify, publish, use, compile, sell, or
" distribute this software, either in source code form or as a compiled
" binary, for any purpose, commercial or non-commercial, and by any
" means.
"
" In jurisdictions that recognize copyright laws, the author or authors
" of this software dedicate any and all copyright interest in the
" software to the public domain. We make this dedication for the benefit
" of the public at large and to the detriment of our heirs and
" successors. We intend this dedication to be an overt act of
" relinquishment in perpetuity of all present and future rights to this
" software under copyright law.
"
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
" EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
" MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
" IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
" OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
" ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
" OTHER DEALINGS IN THE SOFTWARE.
"
" For more information, please refer to <http://unlicense.org/>

" The intention here was to produce a sane vimrc file that works well on
" both Vim and Neovim. For a more minimal configuration that is still
" very good, there is sensible.vim: https://github.com/tpope/vim-sensible

" Get the latest version at
" https://github.com/riceissa/neovim

set nocompatible
set nomodeline
set modelines=0
syntax enable
let mapleader=' '

" Main options
" ======================================================================
set noautoread
set background=light
set backspace=indent,eol,start
set cmdheight=2
set complete-=i
" Show last line instead of the @ column
set display=lastline
set encoding=utf-8
set formatoptions=tcqj
set hidden
set history=10000
" Show invisible characters
set list
set listchars=nbsp:_,tab:>-,trail:@
" Always enable mouse
set mouse=a
set nrformats=hex
set number
set ruler
set scrolloff=5
set sessionoptions-=options
set showcmd
set noshowmatch
" Show mode name on status line
set showmode
set smarttab
set nospell
set spellfile=~/.spell.en.add
if &tabpagemax < 50
  set tabpagemax=50
endif
set tags=./tags;,tags
set title
set ttyfast
set viminfo+=!
set wildmenu
set wildmode=list:longest,full
set wrap

" Indenting options
" -----------------
set autoindent
set nocindent
set nosmartindent
set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4
set shiftround

" Searching options
" -----------------
set nohlsearch
set incsearch
" Sane casing
set ignorecase
set smartcase

" Maps
" ======================================================================
inoremap jj <Esc>
inoremap kk <Esc>
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk
nnoremap gj j
nnoremap gk k
vnoremap gj j
vnoremap gk k
" Make Y act like D and C
nnoremap Y y$
nnoremap Q @@
nnoremap <Enter> o<Esc>
" Easy editing of vimrc
nnoremap <silent> <leader>ev :tabnew $MYVIMRC<CR>
nnoremap <silent> <leader>sv :source $MYVIMRC<CR>
nnoremap <leader>f :tabe `pwd`<CR>
nnoremap <leader>b :Tex<CR>
function! SplitTerminal()
    :lcd %:p:h
    if has('nvim')
        :sp | term
    else
        :shell
    endif
endfunction
nnoremap <leader>c :call SplitTerminal()<CR>
inoremap <C-U> <C-G>u<C-U>
inoremap <C-w> <C-G>u<C-w>

" Tabs
" ----
nnoremap <silent> <C-t> :tabnew<CR>
nnoremap <silent> <C-n> :tabn<CR>
nnoremap <silent> <C-p> :tabp<CR>

" Windows
" -------
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Buffers
" -------
nnoremap K :bn<CR>
nnoremap _ :bp<CR>

" Option toggling (similar to vim-unimpaired)
" -------------------------------------------
nnoremap <silent> coh :set hlsearch! hlsearch?<CR>
nnoremap <silent> col :set list! list?<CR>
nnoremap <silent> con :set number!<CR>
nnoremap <silent> cop :set paste! paste?<CR>
" Make it easy to switch between programming and prose modes
function! ToggleRead()
    if &linebreak
        set nolinebreak list
    else
        set linebreak nolist
    endif
endfunction
nnoremap <silent> cor :call ToggleRead()<CR>
nnoremap <silent> cos :setlocal spell! spell?<CR>
function! ToggleSyntax()
    " See :h syntax for the code
    if exists("g:syntax_on")
        syntax off
    else
        syntax enable
    endif
endfunction
nnoremap <silent> coy :call ToggleSyntax()<CR>

" Format visually selected region to be up to width characters
function! FormatText(width)
    let tempwidth = &textwidth
    let &textwidth=a:width
    normal gvgq
    let &textwidth=tempwidth
endfunction
vnoremap fms <Esc>:call FormatText(72)<CR>
vnoremap fme <Esc>:call FormatText(80)<CR>
vnoremap fmt <Esc>:call FormatText(80)<CR>
vnoremap fmh <Esc>:call FormatText(100)<CR>
vnoremap fmo <Esc>:call FormatText(100)<CR>

" See https://github.com/riceissa/autolink for source
function! PasteLink()
    let link = @+
    let command = 'autolink.py "' . link . '"'
    return system(command)
endfunction
inoremap <C-l> <C-r>=PasteLink()<CR>

" Paste HTML as Pandoc markdown; remember as 'markdown paste'
nnoremap <leader>mp :r !xclip -sel clip -t text/html -o \| pandoc -f html -t markdown<CR>

" Other options
" ======================================================================
" Change pwd to directory of current file
command! CD :lcd %:p:h
" Make <C-c>, <C-v> work as expected
silent !stty -ixon > /dev/null 2>/dev/null
" Get mswin_extract.vim at
" https://github.com/riceissa/neovim/blob/master/mswin_extract.vim
if has('nvim')
    if filereadable(expand("~/.nvim/mswin_extract.vim"))
        source ~/.nvim/mswin_extract.vim
    endif
else
    if filereadable(expand("~/.vim/mswin_extract.vim"))
        source ~/.vim/mswin_extract.vim
    endif
endif

" Leave paste mode after escaping
augroup paste
    autocmd!
    au InsertLeave * set nopaste
augroup END

" {X,HT}ML options
" ----------------
augroup filetype_html
    autocmd!
    autocmd filetype html setlocal shiftwidth=2 softtabstop=2 tabstop=2
    autocmd filetype xhtml setlocal shiftwidth=2 softtabstop=2 tabstop=2
    autocmd filetype xml setlocal shiftwidth=2 softtabstop=2 tabstop=2
augroup END

" Markdown options
" ----------------
augroup filetype_markdown
    autocmd!
    autocmd BufNewFile,BufRead *.md setlocal filetype=markdown
    autocmd BufNewFile,BufRead *.pdc setlocal filetype=markdown
    autocmd BufNewFile,BufRead *.page setlocal filetype=markdown
    autocmd filetype markdown setlocal linebreak nolist
augroup END

" Custom digraphs
" ---------------
" Use Python's ord() to obtain the integer value of a character. Hit
" <C-k> in insert mode then type the characters following 'dig' to produce
" the special character.

" Ellipsis, …
dig el 8230
" Left and right angle brackets, ⟨ ⟩
dig (< 10216
dig <( 10216
dig )> 10217
dig >) 10217
