" Use vim-plug to manage Vim plugins. Follow the instruction at
" https://github.com/junegunn/vim-plug
call plug#begin('~/.vim/plugged')
Plug 'AndrewRadev/splitjoin.vim', {'commit': '9531bfb26257f0d06f7ea2b7ecb4f13095d430ab'}
Plug 'junegunn/gv.vim', {'commit': 'b6bb6664e2c95aa584059f195eb3a9f3cb133994'}
if executable('ctags')
  Plug 'ludovicchabant/vim-gutentags', {'commit': 'aa47c5e29c37c52176c44e61c780032dfacef3dd'}
endif
Plug 'nelstrom/vim-visual-star-search', {'commit': '37259722f45996733fd309add61385a4ad88bdb9'}
Plug 'ntpeters/vim-better-whitespace', {'commit': '86a0579b330b133b8181b8e088943e81c26a809e'}
" Plug 'riceissa/vim-dualist'
Plug 'riceissa/vim-colorschemes'
Plug 'riceissa/vim-pasteurize'
Plug 'riceissa/vim-proselint'
Plug 'riceissa/vim-rsi'
Plug 'riceissa/vim-uniform'
Plug 'tpope/vim-characterize', {'commit': '7fc5b75e7a9e46676cf736b56d99dd32004ff3d6'}
Plug 'tpope/vim-commentary', {'commit': '64a654ef4a20db1727938338310209b6a63f60c9'}
Plug 'tpope/vim-dispatch', {'commit': '6cc2691576f97d43f8751664d1a1a908b99927e5'}
Plug 'tpope/vim-eunuch', {'commit': '67f3dd32b4dcd1c427085f42ff5f29c7adc645c6'}
if !has('win64')
  Plug 'tpope/vim-fugitive', {'commit': '174230d6a7f2df94705a7ffd8d5413e27ec10a80'}
endif
Plug 'tpope/vim-repeat', {'commit': '24afe922e6a05891756ecf331f39a1f6743d3d5a'}
Plug 'tpope/vim-rhubarb', {'commit': 'ee69335de176d9325267b0fd2597a22901d927b1'}
" Plug 'tpope/vim-sleuth', {'commit': 'be69bff86754b1aa5adcbb527d7fcd1635a84080'}
Plug 'tpope/vim-speeddating', {'commit': '5a36fd29df63ea3f65562bd2bb837be48a5ec90b'}
Plug 'tpope/vim-surround', {'commit': '3d188ed2113431cf8dac77be61b842acb64433d9'}
Plug 'tpope/vim-unimpaired', {'commit': '6d44a6dc2ec34607c41ec78acf81657248580bf1'}
Plug 'tpope/vim-abolish', {'commit': 'dcbfe065297d31823561ba787f51056c147aa682'}

" Filetype-specific plugins
" Plug 'fatih/vim-go'
" Plug 'lervag/vimtex', {'for': 'tex'}
Plug 'riceissa/vim-markdown'
Plug 'riceissa/vim-markdownlint'
Plug 'riceissa/vim-mediawiki'
Plug 'nathangrigg/vim-beancount', {'commit': '25bcbc773554b5798d253a1a5fa5de158792f95e'}
Plug 'davidhalter/jedi-vim'
if has('nvim')
  Plug 'alaviss/nim.nvim', { 'for': ['nim'] }
  Plug 'prabirshrestha/asyncomplete.vim', { 'for': ['nim'] }
endif
call plug#end()

" Workaround for https://github.com/tpope/vim-sleuth/issues/29 to override
" sleuth.vim for some filetypes.
" runtime! plugin/sleuth.vim

" The :SpeedDatingFormat command is not available until the speeddating.vim
" file gets loaded. I *could* add my custom formats in an after/plugin/* file
" as recommended in the documentation for speeddating.vim, but I would rather
" not have to deal with yet another config file that I have to make sure to
" copy over correctly on new machines. The following line forces
" speeddating.vim to be loaded right now, rather than after vimrc, so that we
" can use the :SpeedDatingFormat here in the vimrc.
runtime! plugin/speeddating.vim
" Cycle through weekday names with Ctrl-a and Ctrl-x. The conditional is in
" case the plugin has not yet been installed (without the plugin, the
" SpeedDatingFormat command does not exist so the line would cause an error).
if exists("g:loaded_speeddating")
  SpeedDatingFormat %A
endif

" This was the recommended way to install fzf according to the Ubuntu
" documentation given in 'apt show fzf'.
if filereadable("/usr/share/doc/fzf/examples/fzf.vim")
  source /usr/share/doc/fzf/examples/fzf.vim
endif

set ignorecase smartcase noequalalways list
set spellfile=~/.spell.en.utf-8.add
if exists('&inccommand')
  set inccommand=split
endif
if exists('+smoothscroll')
  set smoothscroll
endif

" Quickly find potentially problematic characters (things like non-printing
" ASCII, exotic whitespace, and lookalike Unicode letters).
nnoremap g/ /[^\d32-\d126“”‘’–—§]<CR>

" First seen at http://vimcasts.org/episodes/the-edit-command/ but this
" particular version is modified from
" https://github.com/nelstrom/dotfiles/blob/448f710b855970a8565388c6665a96ddf4976f9f/vimrc#L80
cnoremap %% <C-R><C-R>=getcmdtype() == ':' ? fnameescape(expand('%:h')).'/' : '%%'<CR>

" From Tim Pope
" https://github.com/tpope/tpope/blob/c743f64380910041de605546149b0575ed0538ce/.vimrc#L284
if exists('*strftime')
  inoremap <silent> <C-G><C-T> <C-R>=repeat(complete(col('.'),map(['%F','%B %-d, %Y','%B %Y','%F %a','%F %a %H:%M','%-d %B %Y','%Y-%m-%d %H:%M:%S','%a, %d %b %Y %H:%M:%S %z','%Y %b %d','%d-%b-%y','%a %b %d %T %Z %Y'],'strftime(v:val)')+[localtime()]),0)<CR>
endif

" Break the undo sequence before hitting enter. We need to do the mapping as
" an <expr> map because otherwise Eunuch (which does its own <CR> mapping for
" #! lines at the start of scripts) gets confused.
inoremap <expr> <CR> "<C-G>u<CR>"

" I don't think this is needed anymore, but I'm keeping it for now just in
" case I run into problems later and want to re-enable it.
" set grepformat^=%f:%l:%c:%m

if has('autocmd')
  " Group the autocommands and clear with 'autocmd!' so that if the vimrc is
  " sourced multiple times, we don't end up with duplicate autocommands. See
  " https://learnvimscriptthehardway.stevelosh.com/chapters/14.html for an
  " explanation.
  augroup vimrc
    autocmd!
    " For Gitit
    autocmd BufNewFile,BufRead *.page setlocal filetype=markdown
    autocmd FileType crontab setlocal commentstring=#%s
    autocmd FileType gitconfig setlocal commentstring=#%s
    autocmd FileType matlab setlocal commentstring=%%s
    autocmd FileType c setlocal commentstring=//%s
    autocmd FileType gitcommit,mail,markdown,mediawiki,tex setlocal spell
    autocmd FileType mediawiki let b:surround_{char2nr('w')} = "[[wikipedia:\r|]]"
    autocmd FileType mediawiki let b:surround_{char2nr('r')} = "<ref name=\"\r\" />"
    autocmd FileType mediawiki setlocal includeexpr=substitute(toupper(v:fname[0]).v:fname[1:],'\ ','_','g')
    autocmd FileType mediawiki setlocal suffixesadd=.mediawiki
    autocmd FileType mediawiki setlocal linebreak
    autocmd FileType php setlocal commentstring=//%s
    autocmd FileType haskell syntax match hsLineComment '^#!/usr/bin/env.*$'
    autocmd FileType help,man setlocal nolist nospell
    autocmd FileType help,man,fugitive nnoremap <buffer> <silent> q :q<CR>
    " Modified from :help ft-syntax-omni
    if exists('+omnifunc')
      autocmd FileType * if &omnifunc == '' | setlocal omnifunc=syntaxcomplete#Complete | endif
    endif
    autocmd FileType mail,text,help setlocal comments=fb:*,fb:-,fb:+,n:>
    autocmd FileType make setlocal noexpandtab
    autocmd FileType markdown setlocal expandtab
    " Underscore in Markdown documents are almost always part of emphasis,
    " which should not be considered part of the word. For example, pressing *
    " on an emphasized phrase like '_hello there_' (where the cursor is on the
    " h) should search for 'hello', not '_hello'.
    autocmd FileType markdown setlocal iskeyword-=_
    autocmd FileType mediawiki setlocal omnifunc=mediawikicomplete#Complete
    " There are multiple choices here. Compare the following:
    "     <math>\int_a^b f</math> (big integral, left-justified)
    "     :<math>\int_a^b f</math> (big integral, indented and left-justified)
    "     <math display="inline">\int_a^b f</math> (small integral, left-justified)
    "     <math display="block">\int_a^b f</math> (big integral, centered)
    autocmd FileType mediawiki let b:surround_{char2nr('m')} = "<math>\r</math>"
    autocmd FileType mediawiki let b:surround_{char2nr('M')} = ":<math>\r</math>"
    " In some versions, when Vim is compiled with python3 support but not
    " python support, the omnifunc check above tries to use
    " pythoncomplete#Complete, which doesn't exist since there is no python
    " support. The solution is to force the python3 complete function.
    if has('python3')
      " autocmd FileType python setlocal omnifunc=python3complete#Complete
    endif
    " PHP autoindenting is too smart for its own good
    autocmd FileType php setlocal autoindent indentexpr=
    " The following is for jedi; see :h g:jedi#show_call_signatures. If the
    " cmdheight is 1, the call signatures are not shown.
    autocmd FileType python setlocal cmdheight=2
    " Prevent overzealous autoindent in align environment
    autocmd FileType tex setlocal indentexpr=
    autocmd FileType tex let b:surround_{char2nr('m')} = "\\(\r\\)"
    autocmd FileType tex let b:surround_{char2nr('M')} = "\\[\n\r\n\\]"
    " More aggressively check spelling in LaTeX; see
    " http://stackoverflow.com/questions/5860154/vim-spell-checking-comments-only-in-latex-files
    autocmd FileType tex syntax spell toplevel
    autocmd FileType vim setlocal keywordprg=:help
    " Automatically enter insert mode when switching to a terminal buffer
    " if has('nvim')
    "   autocmd BufEnter term://* startinsert
    " endif
    au User asyncomplete_setup call asyncomplete#register_source({
        \ 'name': 'nim',
        \ 'whitelist': ['nim'],
        \ 'completor': {opt, ctx -> nim#suggest#sug#GetAllCandidates({start, candidates -> asyncomplete#complete(opt['name'], ctx, start, candidates)})}
        \ })
  augroup END
endif

" Fix common typos where one character is stuck to the beginning of the next
" word or the end of the last word.
inoremap <C-G>h <C-G>u<Esc>BxgEpgi
inoremap <C-G>l <C-G>u<Esc>gExpgi

if has('digraphs')
  digraph el 8230
  digraph ./ 8230
  digraph ^\| 8593
  digraph \\ 8726
  digraph \- 8726
  digraph -\ 8726
  digraph \|> 8614
  digraph v\| 8595
  digraph \|- 8866
  digraph \|= 8872
  " Run under exe so that syntax highlighting isn't messed up
  exe 'digraph (/ 8713'
  exe 'digraph (\ 8713'
  exe 'digraph (< 10216'
  exe 'digraph >) 10217'
endif

iabbrev ADd Add

let g:tex_flavor = 'latex'
let g:sql_type_default = 'mysql'
let g:surround_{char2nr('q')} = "“\r”"
let g:surround_{char2nr('Q')} = "‘\r’"
let g:dualist_color_listchars = 1

" Show argument hints when calling functions in the status line rather than in
" the buffer, and wait a while before showing it. This reduces visual clutter.
let g:jedi#show_call_signatures = "2"
let g:jedi#show_call_signatures_delay = 1000
" Don't mess with the popup menu; I like Vim's default completion bindings.
let g:jedi#popup_select_first = 0
let g:jedi#auto_vim_configuration = 0
" Use Vim's usual bindings for completion.
let g:jedi#goto_command = "<C-]>"
let g:jedi#completions_command = "<C-X><C-O>"

let g:strip_whitespace_on_save=1
let g:strip_whitespace_confirm=0
let g:strip_max_file_size = 2000

" I forgot why I needed this...
" nnoremap [s [s<Space><BS>
" nnoremap ]s ]s<BS><Space>

if has('gui_running')
  set guioptions-=m
  set guioptions-=T
endif

" Neovim has an annoying blinking cursor by default (well, it *did* at one
" point, but as of v0.10.4 it doesn't seem to anymore); this turns that off.
" Possibly better to use the following as the check:
" if has('nvim') && $TERM =~# 'screen'
" if has('nvim')
"   set guicursor=n:blinkon0
" endif

" Colorscheme stuff. $DARKMODE is an environment variable that is set in my
" ~/.bashrc that gets set to '1' at night and '0' during the day.
if has('nvim')
  set termguicolors
endif
if exists('$DARKMODE') && $DARKMODE == '1'
  set background=dark
else
  set background=light
  silent! colorscheme issa_light
endif
