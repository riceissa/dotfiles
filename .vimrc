" Get the latest version at
" https://raw.githubusercontent.com/riceissa/dotfiles/master/.vimrc
set nocompatible
if filereadable(expand('~/.vim/plugins.vim'))
    " This includes 'set nocompatible' again
    source ~/.vim/plugins.vim
else
    filetype plugin indent on
endif
" Secure Vim by disabling modelines; see
" http://usevim.com/2012/03/28/modelines/ and
" http://www.techrepublic.com/blog/it-security/turn-off-modeline-support-in-vim/
" for more information
set nomodeline
set modelines=0
syntax enable

" Main options
" ======================================================================
set noautoread
set background=light
set backspace=indent,eol,start
set complete-=i
if filereadable('/usr/share/dict/words')
    set dictionary+=/usr/share/dict/words
endif
" Show last line instead of the @ column
set display=lastline
if &encoding !=? 'utf-8'
    set encoding=utf-8
endif
set formatoptions=tcqj
set hidden
set history=10000
if exists('+langnoremap')
    set langnoremap
endif
set laststatus=2 " Always show status line
set list " Show invisible characters
set listchars=nbsp:_,tab:>\ ,trail:@
set mouse=a " Always enable mouse
set nrformats=hex
set number
set ruler
set sessionoptions-=options
set showcmd
set noshowmatch
set showmode " Show mode name on last line
set smarttab
set spellfile=~/.spell.en.add
set tabpagemax=50
set tags=./tags;,tags
set title
set ttyfast
if $TERM ==? "xterm-256color" || $TERM ==? "screen-256color"
    set t_Co=256
    " Colors
    " ------
    " Color special keys (i.e. those visible with 'list') as in
    " gvim+Solarized
    highlight SpecialKey ctermfg=DarkGray ctermbg=LightGray
    " Less prominent status line
    highlight StatusLine ctermbg=LightGray ctermfg=DarkGray cterm=none
endif
set viminfo+=!
set wildignore+=*.swp,*.pyc
set wildignorecase
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
set ignorecase smartcase " Sane casing

" Maps
" ======================================================================
inoremap jj <Esc>
inoremap kk <Esc>
" Make Y act like D and C
nnoremap Y y$
nnoremap Q @@
nnoremap <Enter> o<Esc>
inoremap <C-u> <C-g>u<C-u>
inoremap <C-w> <C-g>u<C-w>
nnoremap <silent> K <C-^>

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
function! PasteLink(fmt)
    " escape double and single quotes and backslashes to prevent
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
if executable('autolink.py') && has('clipboard')
    " Break up the undo first in case the output is messed up
    inoremap <C-b> <C-G>u<C-r>=PasteLink(&filetype)<CR>
endif

" from Practical Vim
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'

" Other options
" ======================================================================
" Paste HTML as Pandoc markdown
command! MarkdownPaste :r !xclip -sel clip -t text/html -o | pandoc -f html -t markdown

" Change pwd to directory of current file
command! CD :lcd %:p:h

if has('clipboard')
    " quickly grab the whole buffer
    command! Copy :normal gg"+yG``
    command! Clip :normal gg"+yG``
endif

" Leave paste mode after escaping
augroup paste
    autocmd!
    autocmd InsertLeave * set nopaste
augroup END

" Git commit message options
" --------------------------
augroup filetype_gitcommit
    autocmd!
    autocmd filetype gitcommit setlocal spell
augroup END

" {HT,X}ML options
" ----------------
augroup filetype_html
    autocmd!
    autocmd filetype html setlocal shiftwidth=2 softtabstop=2 tabstop=2
    autocmd filetype xhtml setlocal shiftwidth=2 softtabstop=2 tabstop=2
    autocmd filetype xml setlocal shiftwidth=2 softtabstop=2 tabstop=2
augroup END

" LaTeX options
" -------------
let g:tex_flavor='latex'
augroup filetype_tex
    autocmd!
    " prevent overzealous autoindent
    autocmd filetype tex setlocal indentexpr=
augroup END

" Mail options
" ------------
augroup filetype_mail
    autocmd!
    autocmd filetype mail setlocal linebreak nolist spell
augroup END

" Makefile options
" ----------------
augroup filetype_makefile
    autocmd!
    " Makefiles only work with actual tabs
    autocmd filetype make setlocal noexpandtab
augroup END

" Markdown options
" ----------------
augroup filetype_markdown
    autocmd!
    autocmd BufNewFile,BufRead *.md setlocal filetype=markdown
    autocmd BufNewFile,BufRead *.pdc setlocal filetype=markdown
    autocmd BufNewFile,BufRead *.page setlocal filetype=markdown
    autocmd filetype markdown setlocal linebreak nolist spell
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
