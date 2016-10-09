set nocompatible
" Use vim-plug to manage Vim plugins. See https://github.com/junegunn/vim-plug
" for full instructions. Once all Vim config files are in the right places,
" just do :PlugInstall in Vim to install the plugins. The exception is
" YouCompleteMe, which needs to be compiled; see below for more.
call plug#begin('~/.vim/plugged')
Plug 'altercation/vim-colors-solarized' " Only for gvim
Plug 'chrisbra/unicode.vim'
Plug 'fatih/vim-go'
Plug 'justinmk/vim-sneak'
Plug 'majutsushi/tagbar'
Plug 'nelstrom/vim-visual-star-search'
Plug 'riceissa/vim-autolink'
Plug 'riceissa/vim-emacsctrll'
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
" YouCompleteMe doesn't work with just :PlugInstall, so compile it as follows
" after calling :PlugInstall in Vim (which is still necessary since we have to
" clone various repositories).
"     cd ~/.vim/plugged/YouCompleteMe
"     ./install.py --clang-completer
Plug 'Valloric/YouCompleteMe', {'for': ['python', 'java', 'c']}
call plug#end()

" Workaround for https://github.com/tpope/vim-sleuth/issues/29 to override
" sleuth.vim for some filetypes.
runtime! plugin/sleuth.vim

" Resolve disputes between `vim -Nu sensible.vim` and `nvim -u sensible.vim`
if &history < 10000
  set history=10000
endif
set nohlsearch
" From $VIMRUNTIME/vimrc_example.vim @ 97 if you have Vim 7.4.
" See
"   :edit +/langmap $VIMRUNTIME/defaults.vim
" for Vim 8.0.
if has('langmap') && exists('+langnoremap')
  set langnoremap
endif
set listchars=tab:▸\ ,trail:·,nbsp:+
if has('mouse')
  set mouse=a
endif
if has('path_extra')
  setglobal tags=./tags;,tags
endif

set modeline " Debian disables modeline
set number list ignorecase smartcase showcmd noequalalways nojoinspaces
set spellfile=~/.spell.en.add
set wildmode=list:longest,full

" From debian.vim
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc

" Make gq and gw accept a count in visual mode. So 72gq formats as if
" 'textwidth' is 72 regardless of what value it actually is. When no count is
" given, the commands work as usual.
vnoremap <silent> gq :<C-U>call <SID>GQ(v:count, 'gq')<CR>
vnoremap <silent> gw :<C-U>call <SID>GQ(v:count, 'gw')<CR>

function! s:GQ(tw, command)
  if v:count > 0
    let tmp = &tw
    let &tw = a:tw
    exe 'normal! gv' . a:command
    let &tw = tmp
  else
    exe 'normal! gv' . a:command
  endif
endfunction

" TODO: account for &scrolloff
" Also various bugs like 0gj being passed out
nnoremap <expr> gL ':normal ' . (winheight(0) - winline()) . 'gj<CR>'
nnoremap <expr> gH ':normal ' . (winline() - 1) . 'gk<CR>'
nnoremap <expr> gM winheight(0)/2 - winline() > 0 ? ':normal ' . winheight(0)/2 - winline() . 'gj<CR>' : ':normal ' . winheight(0)/2 - winline() . 'gk<CR>'
nnoremap <expr> g<C-D> ':normal ' . (winheight(0) - winline()) . 'gj<CR>' . ':normal ' . (winheight(0) / 2) . 'gj<CR>'
nnoremap <expr> g<C-U> ':normal ' . (winheight(0) / 2) . 'gk<CR>'

" Experimental
inoremap <C-G>h <C-G>u<Esc>BxgEpgi
inoremap <C-G>l <C-G>u<Esc>gExpgi

" Something I like from Acme
command! EditAtRegex call <SID>EditAtRegex()

function! s:EditAtRegex()
  " Convert filename:/regex to :edit +/regex filename
  " Regex will be used as-is and cannot contain whitespace, since we select it
  " using <cWORD>. Use only for simple references.
  " Possibly dangerous in case of filename:dangerous_cmd
  let fname = expand('<cfile>')
  let cmd = get(split(expand('<cWORD>'), ':'), 1, "")
  if cmd !=# ""
    let cmd = "+" . cmd
  endif
  exe ":edit " . cmd . " " . fname
endfunction

if exists("+completefunc")
  " Adapted from :help complete-functions.
  function! CompleteChar(findstart, base)
    if a:findstart
      " locate the start of the word
      let line = getline('.')
      let start = col('.') - 1
      " Assume first that the completion term is a WORD.
      while start > 0 && line[start - 1] =~ '\S'
        let start -= 1
      endwhile
      " The argument passed into DoCompl() here is supposed to be the a:base
      " that a complete function will take on its second round. In other
      " words, we end up doing a "test round" of completion before actually
      " doing it, which is a hack but it's not a big deal as long as the
      " completion list is small. If this sort of thing isn't done, then we
      " run into a problem later where when we make the a:base smaller, it
      " eats up more than it should because start is still the old value. By
      " the way, the col('.')-2 here is because we don't want to include the
      " column the cursor is on, and also because Vim's list slicing is
      " inclusive on the right end.
      if empty(<SID>DoCompl(line[start:col('.')-2]))
        " Completion failed assuming that the completion term is a WORD, so
        " now assume that the completion term is a word and retry.
        let start = col('.') - 1
        while start > 0 && line[start - 1] =~ '\w'
          let start -= 1
        endwhile
      endif
      return start
    else
      return <SID>DoCompl(a:base)
    endif
  endfunction
  set completefunc=CompleteChar
endif

function! s:DoCompl(base)
  let res = []
  if (a:base ==? "date") || (a:base ==? "datetime")
    let date_fmts = [
          \ "%F",
          \ "%B %-d, %Y",
          \ "%-d %B %Y",
          \ "%Y-%m-%d %H:%M:%S",
          \ "%a, %d %b %Y %H:%M:%S %z",
          \ "%Y %b %d",
          \ "%d-%b-%y",
          \ "%a %b %d %T %Z %Y"
          \ ]
    let compl_lst = map(date_fmts, 'strftime(v:val)') + [localtime()]
    for compl in compl_lst
      call add(res, compl)
    endfor
  endif
  let candidates = [
        \ {"word": "…", "menu": "HORIZONTAL ELLIPSIS", "matches": ['...', "ellipsis", "ellipses", "dots", "hellip"]},
        \ {"word": "–", "menu": "EN DASH", "matches": ['en-dash', 'ndash', '--']},
        \ {"word": "—", "menu": "EM DASH", "matches": ['em-dash', 'mdash', '---']},
        \ {"word": "§", "menu": "SECTION SIGN", "matches": ['section', 'SS', '\S']},
        \ {"word": "⟨", "menu": "MATHEMATICAL LEFT ANGLE BRACKET", "matches": ['\langle', 'langle', "<"]},
        \ {"word": "⟩", "menu": "MATHEMATICAL RIGHT ANGLE BRACKET", "matches": ['\rangle', 'rangle', ">"]},
        \ {"word": "×", "menu": "MULTIPLICATION SIGN", "matches": ['\times', 'times', 'x', '*', "multiplication", "multiply"]},
        \ {"word": "·", "menu": "MIDDLE DOT", "matches": ['.M', '~.', '\cdot', 'cdot', 'middot', 'middle_dot', 'middle-dot', 'middledot', '.*', '*.']},
        \ {"word": "©", "menu": "COPYRIGHT SIGN", "matches": ['copyright', '\copyright', '\textcopyright']},
        \ {"word": "∖", "menu": "SET MINUS", "matches": ['setminus', '\setminus', '\\']},
        \ {"word": "∈", "menu": "ELEMENT OF", "matches": ['\in', 'in', '(-', 'isin', 'is_in', 'is-in']},
        \ {"word": "∉", "menu": "NOT AN ELEMENT OF", "matches": ['notin', '\notin', '\not\in', '(-/']},
        \ {"word": "→", "menu": "RIGHTWARDS ARROW", "matches": ['\rightarrow', 'rightarrow', 'right_arrow', 'right-arrow', '->', '>-', 'arrow']},
        \ {"word": "←", "menu": "LEFTWARDS ARROW", "matches": ['\leftarrow', 'leftarrow', 'left_arrow', 'left-arrow', '<-', '-<', 'arrow']},
        \ {"word": "↓", "menu": "DOWNWARDS ARROW", "matches": ['\downarrow', 'downarrow', 'down_arrow', 'down-arrow', 'v|', '|v', 'v-', '-v', 'arrow']},
        \ {"word": "↑", "menu": "UPWARDS ARROW", "matches": ['\uparrow', 'uparrow', 'up_arrow', 'up-arrow', '^|', '|^', '!-', '-!', '^-', '-^', 'arrow']},
        \ {"word": "↦", "menu": "RIGHTWARDS ARROW FROM BAR", "matches": ['\mapsto', 'mapsto', '|>', '|->']},
        \ {"word": "↔", "menu": "LEFT RIGHT ARROW", "matches": ['\leftrightarrow', 'leftrightarrow', '<>', 'left_right_arrow', 'left-right-arrow', '<->', 'arrow']},
        \ {"word": "∞", "menu": "INFINITY", "matches": ['\infty', 'infinity', 'infty', '00', 'oo']},
        \ {"word": '∀', "menu": "FOR ALL", "matches": ['\forall', 'forall', 'for_all', 'for-all', 'V-', 'FA']},
        \ {"word": '∃', "menu": "THERE EXISTS", "matches": ['\exists', 'exists', 'there_exists', 'there-exists', 'TE', 'thereexists']},
        \ {"word": '¬', "menu": "NOT SIGN", "matches": ['-,', 'NO', 'NOT', 'not_sign', 'not-sign']},
        \ {"word": '∧', "menu": "LOGICAL AND", "matches": ['AN', 'AND', '^', 'logicaland', '\wedge', 'wedge', 'logical_and', 'logical-and']},
        \ {"word": '∨', "menu": "LOGICAL OR", "matches": ['OR', 'logicalor', 'logical_or', 'logical-or', 'v', '\vee', 'vee']},
        \ ]
  let typed = substitute(a:base, '\', '\\\\', 'g')
  for cand in candidates
    for m in cand["matches"]
      if m =~? '^\V' . typed
        call add(res, cand)
      endif
    endfor
  endfor
  return res
endfunction

nnoremap Y y$

" With man.vim loaded, <leader>K is more useful anyway
nnoremap K <C-^>
if !has('nvim')
  runtime! ftplugin/man.vim
endif
if has('nvim') && maparg('<Leader>K', 'n') ==# ''
  noremap <Leader>K :Man<CR>
endif

" First seen at http://vimcasts.org/episodes/the-edit-command/ but this
" particular version is from
" https://github.com/nelstrom/dotfiles/blob/448f710b855970a8565388c6665a96ddf4976f9f/vimrc#L80
cnoremap <expr> %% getcmdtype() == ':' ? fnameescape(expand('%:h')).'/' : '%%'

" Credit goes to Tim Pope for the idea
" https://github.com/tpope/tpope/blob/c743f64380910041de605546149b0575ed0538ce/.vimrc#L284
if exists("*strftime")
  inoremap <silent> <C-G><C-T> <C-R>=<SID>ListDatetime()<CR>
  function! s:ListDatetime()
    let date_fmts = [
          \ "%F",
          \ "%B %-d, %Y",
          \ "%Y-%m-%d %H:%M:%S",
          \ "%a, %d %b %Y %H:%M:%S %z",
          \ "%Y %b %d",
          \ "%d-%b-%y",
          \ "%a %b %d %T %Z %Y"
          \ ]
    let compl_lst = map(date_fmts, 'strftime(v:val)') + [localtime()]
    call complete(col('.'), compl_lst)
    return ''
  endfunction
endif

" Quickly find characters that are not printable ASCII, which are sometimes
" undesirable to have in a file. This is best used along with
"     :setlocal nospell hlsearch syntax=OFF
" so that the characters in question stand out.
command! FindNonAscii /[^\d32-\d126]

" From defaults.vim; see also :help :DiffOrig
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
                  \ | wincmd p | diffthis
endif

if &t_Co >= 16
  " Changing ctermbg is useful for seeing tab with :set list
  highlight SpecialKey ctermfg=DarkGray ctermbg=LightGray
endif

let g:tex_flavor='latex'
if has('autocmd')
  augroup vimrc_au
    autocmd!
    autocmd FileType gitcommit,mail,markdown,mediawiki,tex,text setlocal spell
    autocmd InsertEnter * set listchars=tab:▸\ ,nbsp:+
    autocmd InsertLeave * set listchars=tab:▸\ ,trail:·,nbsp:+
    autocmd FileType help,man setlocal nolist nospell
    " Modified from :help ft-syntax-omni
    if exists("+omnifunc")
      autocmd FileType *
              \  if &omnifunc == "" |
              \    setlocal omnifunc=syntaxcomplete#Complete |
              \  endif
    endif
    autocmd FileType mail,text setlocal comments=fb:*,fb:-,fb:+,n:>
    autocmd FileType make setlocal noexpandtab
    autocmd FileType markdown,python setlocal expandtab shiftwidth=4
    autocmd FileType mediawiki noremap <buffer> j gj
    autocmd FileType mediawiki noremap <buffer> k gk
    autocmd FileType mediawiki noremap <buffer> gj j
    autocmd FileType mediawiki noremap <buffer> gk k
    " In Ubuntu 16.04, vim-gtk is compiled with python3 support but not python
    " support. However, the omnifunc check above tries to use
    " pythoncomplete#Complete here, which doesn't exist since there is no
    " python support. The solution is to force the python3 complete function.
    " (This is my guess as to what is going on, and will allow completion to
    " work, but I haven't investigated this issue in detail.)
    if has('python3')
      autocmd FileType python setlocal omnifunc=python3complete#Complete
    endif
    " Prevent overzealous autoindent in align environment
    autocmd FileType tex setlocal indentexpr=
    " More aggressively check spelling in LaTeX; see
    " http://stackoverflow.com/questions/5860154/vim-spell-checking-comments-only-in-latex-files
    autocmd FileType tex syntax spell toplevel
  augroup END
endif

if has('digraphs')
  " Horizontal ellipsis, …
  digraph el 8230
endif

let g:autolink_executable = '/home/issa/projects/autolink/autolink.py'
let g:autolink_download_provider = 'curl -L --silent --compressed'

let g:ycm_filetype_blacklist = {
    \ 'gitcommit': 1,
    \ 'html': 1,
    \ 'mail' : 1,
    \ 'markdown' : 1,
    \ 'mediawiki': 1,
    \ 'notes' : 1,
    \ 'pandoc' : 1,
    \ 'pdc' : 1,
    \ 'tex': 1,
    \ 'text': 1,
    \ 'unite' : 1,
    \}

let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'
let g:EclimCompletionMethod = 'omnifunc'
