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
inoremap <C-u> <C-g>u<C-u>
inoremap <C-w> <C-g>u<C-w>
inoremap <C-r> <C-g>u<C-r>

nnoremap K <C-^>
set hidden number ruler showcmd
set expandtab shiftwidth=4 softtabstop=4 tabstop=4
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
set laststatus=1
set listchars=tab:>\ ,trail:@,nbsp:_
set mouse=a         " Always enable mouse
set nrformats=hex
set sessionoptions-=options
set smarttab
set tabpagemax=50
setglobal tags=./tags;,tags
set ttyfast
set viminfo^=!
set wildmenu

if $TERM ==? "xterm-256color" || $TERM ==? "screen-256color"
    set t_Co=256
    highlight SpecialKey ctermfg=DarkGray ctermbg=LightGray
endif

if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
    runtime! macros/matchit.vim
endif

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
if executable('autolink.py') && has('clipboard')
    " Break up the undo first in case the output is messed up
    inoremap <C-b> <C-G>u<C-r>=PasteLink(&filetype)<CR>
endif

" Paste HTML as Pandoc markdown
if executable('xclip') && executable('pandoc')
    command! MarkdownPaste :r !xclip -sel clip -t text/html -o | pandoc -f html -t markdown
endif

let g:tex_flavor='latex'
if has('autocmd')
    augroup filetype_specific
        autocmd!
        " Leave paste mode after escaping
        autocmd InsertLeave * set nopaste
        autocmd BufNewFile,BufRead *.md,*.page,*.pdc setlocal filetype=markdown
        autocmd BufNewFile,BufRead *.mediawiki setlocal filetype=html
        autocmd filetype gitcommit setlocal spell
        autocmd filetype html,xhtml,xml setlocal shiftwidth=2 softtabstop=2 tabstop=2
        " Prevent overzealous autoindent
        autocmd filetype tex setlocal indentexpr=
        autocmd filetype mail setlocal linebreak nolist spell
        autocmd filetype make setlocal noexpandtab
        autocmd filetype markdown setlocal linebreak nolist spell
    augroup END
endif

" Ellipsis, …
dig el 8230
" Left and right angle brackets, ⟨ ⟩
dig (< 10216
dig <( 10216
dig )> 10217
dig >) 10217
