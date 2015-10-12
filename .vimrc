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

" The intention here is to produce a sane .vimrc that makes both Vim and
" Neovim act in the same way. As such, where Neovim changes a default option
" from that in Vim, this is set explicitly here. For a more minimal
" configuration that is still very good, there is sensible.vim:
" https://github.com/tpope/vim-sensible

" Get the latest version at
" https://raw.githubusercontent.com/riceissa/dotfiles/master/.vimrc

set nocompatible
if filereadable(expand("~/.vim/plugins.vim"))
    " This includes 'set nocompatible' again
    source ~/.vim/plugins.vim
endif
" Secure Vim by disabling modelines; see
" http://usevim.com/2012/03/28/modelines/ and
" http://www.techrepublic.com/blog/it-security/turn-off-modeline-support-in-vim/
" for more information
set nomodeline
set modelines=0
syntax enable
let mapleader=' '

" Main options
" ======================================================================
set noautoread
set background=light
set backspace=indent,eol,start
if has('nvim')
    " Neovim has an extra bar so only two lines for the command line is
    " needed.
    set cmdheight=2
else
    " In Vim, showcmd takes up space in the command line, and, when used
    " along with :make, forces one to hit enter twice when the makefile
    " line is sufficiently long (which is often). The solution is to
    " either make the terminal screen wide enough to accompany both the
    " partial command for showcmd and makefile line (not preferred), or
    " to increase the cmdheight by one.
    set cmdheight=3
endif
set complete-=i
" Show last line instead of the @ column
set display=lastline
set encoding=utf-8
set formatoptions=tcqj
set hidden
set history=10000
if exists('+langnoremap')
    set langnoremap
endif
set laststatus=2
" Show invisible characters
set list
set listchars=nbsp:_,tab:>\ ,trail:@
" Color special keys (i.e. those visible with 'list') as in
" gvim+Solarized
highlight SpecialKey ctermfg=DarkGray ctermbg=LightGray
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
if $TERM == "xterm-256color" || $TERM == "screen-256color"
    set t_Co=256
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
" Sane casing
set ignorecase
set smartcase

" Maps
" ======================================================================
" 'Don't use a map when a command will do. Vim doesn't even have a map
" for :write.' -- Tim Pope
" https://www.reddit.com/r/vim/comments/267vrv/i_am_tim_pope_crafter_of_plugins_ama/chooack
"
" Most maps below just vary the default behavior slightly rather than
" doing completely new things. This makes it easy to program even
" without one's own .vimrc (i.e. it reduces dependence on idiosyncratic
" configuration). Moreover maps are more difficult to remember than
" commands since maps don't benefit from autocompletion. For these
" reasons, maps are kept to a minimum.
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
inoremap <C-u> <C-g>u<C-u>
inoremap <C-w> <C-g>u<C-w>
inoremap <CR> <C-g>u<CR>

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
function! PasteLink(fmt)
    " escape double and single quotes and backslashes to prevent
    " potential attacks against oneself
    let link = substitute(@+, '"', '%22', 'g')
    let link = substitute(link, "'", "%27", "g")
    let link = substitute(link, '\', "%5C", "g")
    let command = "autolink.py --clean --format " . a:fmt . " '" . link . "'"
    return system(command)
endfunction
" Break up the undo first in case the output is messed up
inoremap <C-l> <C-G>u<C-r>=PasteLink('none')<CR>

" Paste HTML as Pandoc markdown; remember as 'markdown paste'
nnoremap <leader>mp :r !xclip -sel clip -t text/html -o \| pandoc -f html -t markdown<CR>

" Other options
" ======================================================================
" Easy editing of vimrc
command! EditVimrc :tabnew $MYVIMRC
command! SourceVimrc :source $MYVIMRC
" Change pwd to directory of current file
command! CD :lcd %:p:h
command! ExplorePwd :edit `pwd`
command! PwdExplore :edit `pwd`

" Saner copy-pasting
" ------------------
" Make <C-c>, <C-x>, and <C-v> work as expected
if has('clipboard')
    vnoremap <C-x> "+x
    vnoremap <C-c> "+y
    " In insert mode, just use <C-o><C-v> or <C-\><C-o><C-v>
    nnoremap <C-v> "+gP
else
    vnoremap <C-x> x
    vnoremap <C-c> y
    nnoremap <C-v> gP
endif
" Since <C-v> in normal mode no longer works, we define commands to
" allow visual block mode. This avoids the usual remapping to <C-q>
" (which creates problems since Vim never sees it, and even if the
" signal is passed to Vim, if one uses Vim within tmux, then tmux still
" intercepts it(?), and so on). Note that <C-c> and <C-v> require no
" similar definitions of commands, since <C-x> has no meaning in visual
" mode, and <C-c> in visual mode is equivalent to one or two escapes(?).
command! VisualBlock normal! <C-v>
command! Vb normal! <C-v>
command! VB normal! <C-v>

" Leave paste mode after escaping
augroup paste
    autocmd!
    au InsertLeave * set nopaste
augroup END

" {HT,X}ML options
" ----------------
augroup filetype_html
    autocmd!
    autocmd filetype html setlocal shiftwidth=2 softtabstop=2 tabstop=2
    autocmd filetype xhtml setlocal shiftwidth=2 softtabstop=2 tabstop=2
    autocmd filetype xml setlocal shiftwidth=2 softtabstop=2 tabstop=2
    autocmd filetype html inoremap <buffer> <C-l> <C-G>u<C-r>=PasteLink('html')<CR>
augroup END

" LaTeX options
" -------------
let g:tex_flavor='latex'
augroup filetype_tex
    " Make visually selected region be mathematically typeset
    autocmd filetype tex vnoremap <buffer> <silent> ma <esc>`>a\)<esc>`<i\(<esc>
    autocmd filetype tex inoremap <buffer> <C-l> <C-G>u<C-r>=PasteLink('latex')<CR>
    " see https://superuser.com/questions/865513/turn-off-vims-filetype-indent-for-a-specific-filetype
    autocmd BufRead,BufNewFile *.tex filetype indent off
augroup END

" Mail options
" ------------
augroup filetype_mail
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
    autocmd filetype markdown inoremap <buffer> <C-l> <C-G>u<C-r>=PasteLink('markdown')<CR>
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
