set nocompatible
" Use vim-plug to manage Vim plugins. See https://github.com/junegunn/vim-plug
" for full instructions. Once all Vim config files are in the right places,
" just do :PlugInstall in Vim to install the plugins. The exception is
" YouCompleteMe, which needs to be compiled; see below for more.
call plug#begin('~/.vim/plugged')
Plug 'altercation/vim-colors-solarized' " Only for gvim
Plug 'chrisbra/unicode.vim'
Plug 'fatih/vim-go'
Plug 'junegunn/fzf', {'dir': '~/.fzf', 'do': './install --all'}
Plug 'junegunn/gv.vim'
Plug 'ludovicchabant/vim-gutentags'
Plug 'justinmk/vim-sneak'
Plug 'lervag/vimtex', {'for': 'tex'}
Plug 'majutsushi/tagbar'
Plug 'nelstrom/vim-visual-star-search'
Plug 'riceissa/vim-autolink'
Plug 'riceissa/vim-emacsctrll'
Plug '~/projects/vim-longmove'
Plug 'riceissa/vim-markdown'
Plug 'riceissa/vim-markdown-paste'
Plug 'riceissa/vim-mediawiki'
Plug 'riceissa/vim-more-toggling'
Plug 'riceissa/vim-pdf-text-tools'
Plug 'riceissa/vim-rsi'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-characterize'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
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
Plug 'Valloric/YouCompleteMe', {'for': ['python']}
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
set spellfile=~/.spell.en.utf-8.add
set wildmode=list:longest,full
set ttimeoutlen=50

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

" Experimental
" ------------------------------------------------------------------------
" nnoremap <silent> <expr> gH winline() - 1 - &scrolloff > 0
"       \ ? ':<C-U>normal! ' . (winline() - 1 - &scrolloff) . 'gkg^<CR>'
"       \ : 'g^'
" vnoremap <silent> <expr> gH winline() - 1 - &scrolloff > 0
"       \ ? ':<C-U>normal! gv' . (winline() - 1 - &scrolloff) . 'gkg^<CR>'
"       \ : 'g^'
" noremap <silent> <expr> gM winline() < (winheight(0)+1)/2
"       \ ? ':<C-U>normal! ' . ((winheight(0)+1)/2 - winline()) . 'gjg^<CR>'
"       \ : winline() == (winheight(0)+1)/2
"       \         ? 'g^'
"       \         : ':normal! ' . (winline() - (winheight(0)+1)/2) . 'gkg^<CR>'
" noremap <silent> <expr> gL winheight(0) - winline() - &scrolloff > 0
"       \ ? ':<C-U>normal! ' . (winheight(0) - winline() - &scrolloff) . 'gjg^<CR>'
"       \ : 'g^'

" Try to make gH gM gL g<C-E> g<C-Y> g<C-D> g<C-U> g<C-F> g<C-B>
" These are still buggy
nmap <expr> g<C-D> 'gL' . ':normal! ' . (winheight(0) / 2) . 'gjg^<CR>'
nmap <expr> g<C-U> 'gH' . ':normal! ' . (winheight(0) / 2) . 'gkg^<CR>'
nmap g<C-E> gLgjgH
nmap g<C-Y> gHgk

" Fix common typos where one character is stuck to the beginning of the next
" word or the end of the last word.
inoremap <C-G>h <C-G>u<Esc>BxgEpgi
inoremap <C-G>l <C-G>u<Esc>gExpgi

" Something I like from Acme. The problem with this is that it's only useful
" when people refer to files using "filename:/regex", and I haven't
" encountered a lot of this myself.
command! EditAtRegex call <SID>EditAtRegex(":edit")
command! SplitAtRegex call <SID>EditAtRegex(":split")
command! VsplitAtRegex call <SID>EditAtRegex(":vsplit")
command! TabeditAtRegex call <SID>EditAtRegex(":tabedit")

function! s:EditAtRegex(edit)
  " Convert filename:/regex to :edit +/regex filename
  " Regex will be used as-is and cannot contain whitespace, since we select it
  " using <cWORD>. Use only for simple references.
  " Possibly dangerous in case of filename:dangerous_cmd
  let fname = expand('<cfile>')
  let cmd = get(split(expand('<cWORD>'), ':'), 1, "")
  if cmd !=# ""
    let cmd = "+" . cmd
  endif
  exe a:edit . " " . cmd . " " . fname
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
  " Credit for the datetime completion idea goes to Tim Pope
  " https://github.com/tpope/tpope/blob/c743f64380910041de605546149b0575ed0538ce/.vimrc#L284
  if exists("*strftime") && ((a:base ==? "date") || (a:base ==? "datetime"))
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
        \ {"word": '≡', "menu": "IDENTICAL TO", "matches": ['identical', 'equivalent', '3-', '3=', '=3', '-3']},
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

" Now that I have a unified way to search for common Unicode characters
" (unicode.vim and my own completefunc), I'm not sure I'll need this digraph
" anymore. Keeping it in experimental; if I don't use it for a while I'll just
" remove it in favor of completefunc.
if has('digraphs')
  " Horizontal ellipsis, …
  digraph el 8230
  digraph ./ 8230
  digraph ^\| 8593
  digraph \\ 8726
  digraph \- 8726
  digraph -\ 8726
  digraph \|> 8614
  " Run under exe so that syntax highlighting isn't messed up
  exe 'digraph (/ 8713'
  exe 'digraph (\ 8713'
  exe 'digraph (< 10216'
  exe 'digraph >) 10217'
endif

cnoremap <expr> <C-X><C-U> '<C-F>i<C-R>=<SID>CommandlineComplete("' . getcmdtype() . '")<CR>'

function! s:CommandlineComplete(cmdtype)
  if a:cmdtype == ':'
    " You'd think expand('%:h') would work, but the commandline window itself has
    " a filename and directory ('.') assigned to it, so you need to use the
    " previous buffer.
    let compl_lst = [
          \ {"word": fnameescape(expand('#:h')).'/'},
          \ {"word": 'exe "$normal o\<esc>" | g/./,/^$/join | %s/\s\+$//', "menu": "Join each para into one line"},
          \ {"word": '%s/\s\+$//', "menu": "Strip trailing whitespace"},
          \ ]
    call complete(col('.'), compl_lst)
  elseif a:cmdtype == '/' || a:cmdtype == '?'
    let compl_lst = [
          \ {"word": '[^\d32-\d126]', "menu": "Not printable ASCII"},
          \ {"word": '\s\+$', "menu": "Trailing whitespace"},
          \ {"word": '\c\<todo\>', "menu": "TODO"},
          \ ]
    call complete(col('.'), compl_lst)
  endif
  return ''
endfunction

" %% and :FindNonAscii are sort of the "old way" to do things. I'm wondering
" if commandline mode <C-X><C-U> could replace both of them in a unified way.
" Keeping both in the experimental section to see what I prefer over time.

" First seen at http://vimcasts.org/episodes/the-edit-command/ but this
" particular version is from
" https://github.com/nelstrom/dotfiles/blob/448f710b855970a8565388c6665a96ddf4976f9f/vimrc#L80
cnoremap <expr> %% getcmdtype() == ':' ? fnameescape(expand('%:h')).'/' : '%%'

" Quickly find characters that are not printable ASCII, which are sometimes
" undesirable to have in a file. This is best used along with
"     :setlocal nospell hlsearch syntax=OFF
" so that the characters in question stand out.
command! FindNonAscii /[^\d32-\d126]

" From justinmk/config. See https://github.com/tpope/vim-rsi/pull/17/files for
" more details.
function! s:ctrl_u() abort
  if getcmdpos() > 1
    let @- = getcmdline()[:getcmdpos()-2]
  endif
  return "\<C-U>"
endfunction

function! s:ctrl_w_before() abort
  let s:cmdline = getcmdpos() > 1 ? getcmdline() : ""
  return "\<C-W>"
endfunction

function! s:ctrl_w_after() abort
  if strlen(s:cmdline) > 0
    let @- = s:cmdline[(getcmdpos()-1) : (getcmdpos()-2)+(strlen(s:cmdline)-strlen(getcmdline()))]
  endif
  return ""
endfunction

cnoremap <expr> <C-U> <SID>ctrl_u()
cnoremap <expr> <SID>(ctrl_w_before) <SID>ctrl_w_before()
cnoremap <expr> <SID>(ctrl_w_after) <SID>ctrl_w_after()
cmap   <script> <C-W> <SID>(ctrl_w_before)<SID>(ctrl_w_after)
cnoremap        <C-Y> <C-R>-

inoremap <C-G><C-T> <C-R>=<SID>ListDate()<CR>
function! s:ListDate()
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
    call complete(col('.'), compl_lst)
    return ''
endfunction

inoremap <C-G><C-V> "<C-O>"+gp<C-O>A"

function! s:BrowseNewTab(progname)
  tabnew
  set bt=nofile
  setl nonumber
  exec "0r !/home/issa/fetch-page.sh " . a:progname
  let b:url = @+
  1
endfunction
command! BrowseNewTab :call <SID>BrowseNewTab("wget")
command! BrowseNewTabCurl :call <SID>BrowseNewTab("curl")

inoremap <expr> <C-C> pumvisible() ? '<C-E>' : '<C-C>'

if has('clipboard')
  " See $VIMRUNTIME/autoload/paste.vim
  nnoremap <C-V> "=@+.'xy'<CR>gPFx"_2x
  inoremap <C-V> <C-G>ux<Esc>"=@+.'xy'<CR>gPFx"_2x"_s
  vnoremap <C-V> "-c<Esc>gix<Esc>"=@+.'xy'<CR>gPFx"_2x"_x

  vnoremap <C-C> "+y
endif

" End of experimental
" ------------------------------------------------------------------------

nnoremap Y y$

" With man.vim loaded, <leader>K is more useful anyway
nnoremap K <C-^>
if !has('nvim')
  runtime! ftplugin/man.vim
endif
if has('nvim') && maparg('<Leader>K', 'n') ==# ''
  noremap <Leader>K :Man<CR>
endif

if exists('&inccommand')
  set inccommand=split
endif

" From defaults.vim; see also :help :DiffOrig
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
                  \ | wincmd p | diffthis
endif

function! s:ColorListChars()
  if &t_Co >= 16
    " Changing ctermbg is useful for seeing tab with :set list
    if &background ==# "dark"
      highlight SpecialKey ctermfg=LightGray ctermbg=DarkGray
    else
      highlight SpecialKey ctermfg=DarkGray ctermbg=LightGray
    endif
  endif
endfunction
call <SID>ColorListChars()

let g:tex_flavor='latex'
if has('autocmd')
  augroup my_init
    autocmd!
    autocmd FileType crontab setlocal commentstring=#%s
    autocmd FileType gitcommit,mail,markdown,mediawiki,tex setlocal spell
    autocmd InsertEnter * set listchars=tab:▸\ ,nbsp:+
    autocmd InsertLeave * set listchars=tab:▸\ ,trail:·,nbsp:+
    autocmd OptionSet background call <SID>ColorListChars()
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
    " sleuth.vim usually detects 'shiftwidth' as 2, though this depends on how
    " the Markdown is written.
    autocmd FileType markdown setlocal expandtab shiftwidth=4
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
    autocmd FileType tex let b:surround_{char2nr('m')} = "\\(\r\\)"
    autocmd FileType tex let b:surround_{char2nr('M')} = "\\[\n\r\n\\]"
    " More aggressively check spelling in LaTeX; see
    " http://stackoverflow.com/questions/5860154/vim-spell-checking-comments-only-in-latex-files
    autocmd FileType tex syntax spell toplevel
  augroup END
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
let g:ycm_python_binary_path = '/usr/bin/python3'
let g:EclimCompletionMethod = 'omnifunc'
