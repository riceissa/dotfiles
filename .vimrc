if !has('nvim')
  " This is the recommended way to load defaults.vim; see :help defaults.vim
  unlet! skip_defaults_vim
  source $VIMRUNTIME/defaults.vim

  silent! packadd! editorconfig
  silent! packadd! comment
  runtime ftplugin/man.vim
  setglobal keywordprg=:Man
endif

" Make the escape key more responsive
set ttimeout ttimeoutlen=50

" There are two kinds of searches in my experience:
"   1. Searching for navigation: I have a specific spot I want to go to,
"      either on the current visible part of the screen, or off-screen, and I
"      just want to go there. This is no different than using some other
"      motion like f<char> or W, or moving around with Ctrl-f, etc.
"   2. Searching to find all of something: I maybe want to see all the places
"      where a variable is being used, and so I want to see all the instances
"      of the searched string.
" In my experience, (1) is a lot more frequent than (2), but only (2) benefits
" from having matches highlighted. So having hlsearch turned on by default
" (which is only on Neovim) is distracting, because I usually just want to put
" my cursor in a particular spot and go on with what I was trying to do.
set nohlsearch

set ignorecase smartcase
set scrolloff=2
set laststatus=2
set nojoinspaces

" I kind of like the discipline of always saving buffers before making them no
" longer visible on the screen. But leaving 'hidden' off turns out to have a
" nasty side effect which is that all of the undo history for a buffer gets
" forgotten when it goes out of view!
set hidden

set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
set belloff=all
set autoindent

" Tab-completing in command mode in Vim by default shows matches horizontally,
" meaning only a few matches can be shown on the screen. Having 'pum' in the
" following option makes matches be displayed vertically instead (just like in
" insert mode), allowing more matches to be shown. Including 'tagfile' shows
" the kind and location of tag when doing :tag <Ctrl-D> which seems helpful,
" but I mostly only included it because it's included by default in Neovim.
set wildoptions=pum,tagfile

" Vim's default C indenting options for switch cases are kind of insane in my
" opinion. The following two options make it so that switch cases go from
" Vim's default of:
"     switch (a) {
"         case 1: {
"                     a++;
"                 } break;
"     }
" to:
"     switch (a) {
"     case 1: {
"         a++;
"     } break;
"     }
set cinoptions=l1,:0

" Fedora's /etc/vimrc sets this to a weird value, so reset it to Vim's default
set viminfo&

if has('mouse')
  set mouse=nv
  if !has('nvim') && exists('$TMUX')
    " Makes mouse dragging work in Vim under tmux
    set ttymouse=xterm2
  endif
endif
if exists('+smoothscroll')
  set smoothscroll
endif

" Show the number of matches when searching
set shortmess-=S

" I kind of prefer that when I toggle buffers with Ctrl-^ the cursor doesn't
" move when I toggle back to the buffer I was on. However, this also changes
" the behavior of a bunch of other movements, which I might not like.
set nostartofline

" This is the Neovim default. Basically what it means is that first, Vim will
" search for the tags file in the directory that the current file is in, i.e.
" whatever :echo expand('%:h') outputs (that's what the "./" means) and then
" if it can't find a tags file there, it will keep recursing upwards from that
" directory until it finds a tags file (that's what the ";" means). If it
" still can't find a tags file, it will now just look in the current working
" directory, i.e. whatever :pwd outputs (that's what the plain "tags" without
" the "./" means), but it won't keep recursing upward from the current working
" directory (note the absence of ";"). For more information, see:
"     :help file-searching for what the semicolon means
"     :help tags-option for what the ./ means
" I like this because sometimes I want to navigate via tags when I am editing
" a file that isn't part of my current project/working directory. If so, I
" wouldn't want Vim to use the tags file associated with my current project;
" instead, I want Vim to prioritize the tags file that is near where the file
" is.
setglobal tags=./tags;,tags

" For consistency with C and D
nnoremap Y y$

" Don't highlight strings in comments. This one turned out to have false
" positives which was very annoying. For example, in a C file if you have a
" comment that looks like:
"     // you'll ... 'blah'
" then everything between the first two single quotes (rather than everything
" between the second and third single quotes) is highlighted in a different
" color than the rest of the comment.
unlet! c_comment_strings

" These are basically taken from Tim Pope's rsi.vim, but I didn't want to use
" any plugins (which would increase the complexity of my Vim setup), so I just
" reimplemented the subset of mappings that I find particularly useful.
inoremap <C-A> <C-O>^
inoremap <C-X><C-A> <C-A>
cnoremap <C-A> <Home>
cnoremap <C-X><C-A> <C-A>
inoremap <expr> <C-E> col(".") >= col("$") ? "<C-E>" : "<End>"
inoremap <expr> <C-F> col(".") >= col("$") ? "<C-F>" : "<Right>"
cnoremap <expr> <C-F> getcmdpos() > strlen(getcmdline()) ? &cedit : "<Right>"
inoremap <C-B> <Left>
cnoremap <C-B> <Left>
inoremap <expr> <C-D> col(".") >= col("$") ? "<C-D>" : "<Del>"
cnoremap <expr> <C-D> getcmdpos() > strlen(getcmdline()) ? "<C-D>" : "<Del>"

if has('nvim-0.10')
  " I find the new default Neovim theme to be too low-contrast and also find
  " all the colors blending together, so go back to the default Vim theme.
  colorscheme vim
  set notermguicolors
endif

if has('autocmd')
  augroup vimrc
    autocmd!
    autocmd BufNewFile,BufRead /etc/nginx/* setfiletype nginx

    " Make shebang lines be highlighted as comments in Haskell files, so that
    " Haskell files can be used as scripts, with '#!/usr/bin/env runhaskell'.
    " The default highlighting makes the line all red, which is visually
    " jarring.
    autocmd FileType haskell syntax match hsLineComment '^#!.*'

    " By default, Go and Rust files have formatprg=gofmt and
    " formatprg=rustfmt, respectively. This may seem ideal, but actually, I
    " prefer to run gofmt/rustfmt in bulk via gofmt -w or rustfmt src, and
    " also gofmt and rustfmt don't format long comments, and comments are
    " pretty much the only thing I use commands like gq for. So basically gq
    " becomes useless in Go and Rust files, which is not what I want. By
    " setting this to be empty, the default Vim formatter takes over, and
    " comment formatting works as usual.
    autocmd FileType go,rust setlocal formatprg=

    " Vim tries to be smart by setting the makeprg for Rust files to use Cargo
    " if Cargo is detected and otherwise use rustc. However, when it uses
    " rustc, it tries to run rustc on the current file, which is annoying
    " (since the file I happen to be editing may not be the main/entrypoint of
    " the project). For now, I am setting it back to just 'make', as I will
    " probably usually have a makefile.
    autocmd FileType rust if &makeprg =~# '^rustc ' | setlocal makeprg=make | endif

    " Vim sets the textwidth to 99 for Rust files by default, which I find
    " annoying (I prefer either 80 if I'm formatting comments or no hard limit
    " so that my lines don't unpredictably wrap, so a limit of 99 is both too
    " large and too small!).
    autocmd FileType rust setlocal textwidth=0

    " Underscores in Markdown files usually mean emphasis, so should not be
    " counted as part of the word. This makes searching for emphasized phrases
    " work with motions like *. For example, if a string like _hello world_
    " appears in a Markdown document, pressing * when the cursor is on the 'h'
    " would by default search for the string "_hello". Having the following
    " autocommand makes it search for "hello" instead, producing symmetry with
    " *-style emphasis like *hello world*.
    autocmd FileType markdown setlocal iskeyword-=_

    autocmd FileType gitcommit setlocal spell
    autocmd FileType c,php,glsl setlocal commentstring=//\ %s
  augroup END

  " This disables the annoying explanation that pops up every time you press
  " Ctrl-f in command mode.
  if exists('#vimHints')
    autocmd! vimHints
  endif

  " Fedora's /etc/vimrc has a broken implementation of :help restore-cursor
  " that does not check whether the buffer is a git commit window, so clear
  " out the augroup. A working implementation is already in defaults.vim, so I
  " don't know why they decided that adding it in again was a good idea.
  if exists('#fedora')
    autocmd! fedora
  endif

  " From :help restore-cursor on Neovim. Vim already has this in defaults.vim
  " which was sourced above.
  if has('nvim')
    augroup RestoreCursor
      autocmd!
      autocmd BufReadPre * autocmd FileType <buffer> ++once
        \ let s:line = line("'\"")
        \ | if s:line >= 1 && s:line <= line("$") && &filetype !~# 'commit'
        \      && index(['xxd', 'gitrebase'], &filetype) == -1
        \ |   execute "normal! g`\""
        \ | endif
    augroup END
  endif
endif

" See :help ft-python-indent for what these mean
let g:python_indent = {}
let g:python_indent.open_paren = 'shiftwidth()'
let g:python_indent.closed_paren_align_last_line = v:false
