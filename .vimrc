set nocompatible
if filereadable(expand('~/.vim/plugins.vim'))
    source ~/.vim/plugins.vim
else
    filetype plugin indent on
endif
set nomodeline modelines=0
syntax enable

" It's too easy to do something unexpected with these commands, so break the
" undo sequence beforehand
inoremap <C-u> <C-g>u<C-u><C-g>u
inoremap <C-w> <C-g>u<C-w><C-g>u
inoremap <C-r> <C-g>u<C-r>

inoremap <Up> <C-g>u<Up>
inoremap <Down> <C-g>u<Down>
inoremap <Left> <C-g>u<Left>
inoremap <Right> <C-g>u<Right>
inoremap <C-r>+ <C-g>u<C-\><C-o>"+gP
inoremap <C-r>* <C-g>u<C-\><C-o>"*gP

" Mappings that conflict with the muscle memory from using default Vim
inoremap <C-f> <C-g>u<Right>
inoremap <C-b> <C-g>u<Left>
inoremap <C-l> <C-g>u<C-o>zz
" With man.vim loaded, <leader>K is more useful anyway
nnoremap K <C-^>
nnoremap Y y$

let mapleader = ' '
nnoremap <C-l> :noh<CR><C-l>
nnoremap <leader>y :%y +<CR>
vnoremap <leader>y "+y
" quickly fix a form of typo I often make
nnoremap <leader>f F<Space>xpA
nnoremap <leader>F F<Space>gExpA
nnoremap <leader>m :update \| !make<CR><CR>
nnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>p "+p
cnoremap <expr> %% getcmdtype() == ':' ? fnameescape(expand('%:h')).'/' : '%%'
set hidden number ruler showcmd list noequalalways nojoinspaces
set expandtab shiftwidth=4 softtabstop=4 tabstop=4
set matchpairs+=<:>,“:”,«:»
set spellfile=~/.spell.en.add
set wildmode=list:longest,full
set ignorecase smartcase

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
set laststatus=2
set listchars=tab:>\ ,trail:@,nbsp:_
set mouse=a         " Always enable mouse
set nrformats=hex
set sessionoptions-=options
set smarttab
set tabpagemax=50
if has('path_extra')
    setglobal tags-=./tags tags-=./tags; tags^=./tags;
endif
set ttyfast
set viminfo^=!
set wildmenu

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

function! ToggleSyntax()
    " See :h syntax for the code
    if exists("g:syntax_on")
        syntax off
    else
        syntax enable
    endif
endfunction
nnoremap <silent> coy :call ToggleSyntax()<CR>

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

" Paste HTML as Pandoc markdown
if executable('xclip') && executable('pandoc')
    command! MarkdownPaste :r !xclip -sel clip -t text/html -o | pandoc -f html -t markdown
    " The following one is useful when pasting from a messy site (e.g. Quora).
    " After :MarkdownCleanPaste, doing :%s/\\\n\\/\r/ also helps.
    command! MarkdownCleanPaste :r !xclip -sel clip -t text/html -o | pandoc -f html -t markdown-raw_html-native_divs-native_spans-link_attributes --wrap=none
endif

" vim-unimpaired has better HTML escaping, but this is for when I don't have
" plugins
command! HTMLEscape :%s/&/\&amp;/ge | %s/</\&lt;/ge | %s/>/\&gt;/ge

" filter text pasted from PDFs, so that formatting is suitable; progress
" ongoing; join must be called at the very end because vim assigns <line1> and
" <line2> when the command is invoked, so we can't change the boundaries of the
" line markers; for the same reason, we can't regex replace new lines
command! -range FilterPDFText silent <line1>,<line2>s/$/ /e | silent <line1>,<line2>s/\-\s\+$//e | silent <line1>,<line2>s/\s\+/ /ge | silent <line1>,<line2>s/^\s\+//e | <line1>,<line2>join!

nnoremap <leader>q :'{,'}FilterPDFText<CR>:s/\s\+$//e<CR>O<Esc>jo<Esc>kgqip

runtime! ftplugin/man.vim

let g:tex_flavor='latex'
if has('autocmd')
    augroup filetype_specific
        autocmd!
        " Leave paste mode after escaping
        autocmd InsertLeave * set nopaste
        autocmd BufNewFile,BufRead *.md,*.page,*.pdc setlocal filetype=markdown
        autocmd BufNewFile,BufRead *.mediawiki setlocal filetype=mediawiki
        autocmd BufNewFile,BufRead */itsalltext/*wikipedia* setlocal filetype=mediawiki
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
        autocmd FileType mail setlocal linebreak nolist spell
        autocmd FileType make setlocal noexpandtab
        autocmd FileType man setlocal nolist
        autocmd FileType markdown setlocal syntax=markdown.pandoc
        autocmd FileType markdown setlocal linebreak nolist spell textwidth=80
        autocmd FileType markdown setlocal nonumber showbreak=\\
        " make this more like visual-star when I get a chance
        autocmd FileType markdown nnoremap <buffer> <C-]> "zya[/\V<C-r>z<CR>
        autocmd FileType mediawiki nnoremap <buffer> j gj
        autocmd FileType mediawiki nnoremap <buffer> k gk
        autocmd FileType mediawiki vnoremap <buffer> j gj
        autocmd FileType mediawiki vnoremap <buffer> k gk
        autocmd FileType mediawiki nnoremap <buffer> gj j
        autocmd FileType mediawiki nnoremap <buffer> gk k
        autocmd FileType mediawiki vnoremap <buffer> gj j
        autocmd FileType mediawiki vnoremap <buffer> gk k
        " modified from $VIM/vim74/syntax/mail.vim
        autocmd FileType markdown syn match markdownURL contains=@NoSpell `\v<(((https?|ftp|gopher)://|(mailto|file|news):)[^' 	<>")]+|(www|web|w3)[a-zA-Z0-9_-]*\.[a-zA-Z0-9._-]+\.[^' 	<>")]+)[a-zA-Z0-9/]`
        autocmd FileType mediawiki syn region mediawikiRef start="\v\<ref[^>/]*\>?" end="\v(\<\/ref\>|/\>)" contains=@NoSpell
        hi def link mediawikiRef Comment
        hi def link markdownURL String
        autocmd FileType mediawiki setlocal spell
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
