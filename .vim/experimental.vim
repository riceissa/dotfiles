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

" From defaults.vim; see also :help :DiffOrig
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
                  \ | wincmd p | diffthis
endif

nnoremap <C-N> gj
nnoremap <C-P> gk

" Fix common typos where one character is stuck to the beginning of the next
" word or the end of the last word.
inoremap <C-G>h <C-G>u<Esc>BxgEpgi
inoremap <C-G>l <C-G>u<Esc>gExpgi

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
" particular version is modified from
" https://github.com/nelstrom/dotfiles/blob/448f710b855970a8565388c6665a96ddf4976f9f/vimrc#L80
cnoremap %% <C-R><C-R>=getcmdtype() == ':' ? fnameescape(expand('%:h')).'/' : '%%'<CR>

" This map makes it easier to search across linebreaks. If you want to
" search for "hello there" but there might be a linebreak in between
" the "hello" and the "there", you will have to do something like
" "hello[ \n]there". But what about other whitespace? Okay, you say, I'll
" search for "hello\_s\+there" to catch one or more whitespace characters,
" including newlines. But what if the phrase occurs in a comment? In that case
" there might be a comment character in front of the "there". And what if this
" is a Markdown document and the phrase occurs inside a blockquote? You don't
" *really* want to search for
" "hello\(\_s\+\|\V<!--\m\|\V-->\m\|\V>\m\)\+there" each time, do you?
" Instead, with this map, just type "hello" then <C-X><Space> then "there".
cnoremap <expr> <C-X><Space> "<C-R>=<SID>InclusiveSpace('" . getcmdtype() . "')<CR>"
function! s:InclusiveSpace(cmdtype)
  " TODO also get 'm' (and others?) from &comments.
  let l:result = '\(\_s\+'
  if &commentstring !=# ""
    " Try to get the parts of the commentstring, e.g. "<!--" and "-->" for
    " HTML, "/*" and "*/" for C.
    for l:cmt in split(&commentstring, '%s')
      if l:cmt !=# ""
        " Strip whitespace
        let l:cmt = substitute(l:cmt, '^\s*\(.\{-}\)\s*$', '\1', '')
        let l:result .= '\|\V' . escape(l:cmt, a:cmdtype.'\') . '\m'
      endif
    endfor
  endif
  if &comments !=# ""
    for l:cmt in split(&comments, ',')
      if l:cmt =~# "n:"
        " Strip the "n:"
        let l:cmt = strpart(l:cmt, 2)
        " Strip whitespace
        let l:cmt = substitute(l:cmt, '^\s*\(.\{-}\)\s*$', '\1', '')
        let l:result .= '\|\V' . escape(l:cmt, a:cmdtype.'\') . '\m'
      endif
    endfor
  endif
  let l:result .= '\)\+'
  return l:result
endfunction

" Quickly find potentially problematic characters (things like non-printing
" ASCII, exotic whitespace, and lookalike Unicode letters). This is best used
" along with
"     :setlocal nospell hlsearch syntax=OFF
" so that the characters in question stand out.
nnoremap g/ /[^\d32-\d126“”‘’–—§]<CR>

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
          \ "%F %a",
          \ "%F %a %H:%M",
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

inoremap <C-G><C-W> <C-\><C-O>"-dB
inoremap <C-G><C-K> <C-\><C-O>"-D
inoremap <C-G><C-D> <C-\><C-O>"-dE
" Reverse the effects of 'textwidth' in insert mode. This is useful if most of
" the lines in a file have one textwidth but a couple have a different one
" (examples: a Markdown file that has a textwidth of 79 but where link
" references have unlimited line length so that they can be sorted; a Vim
" script file that has a textwidth of 78 but where you want one particular
" line to be just a little longer; a Python file that has a textwidth of 0 but
" where you want to format docstrings as you write them). When 'textwidth' is
" disabled, it will format the current line; otherwise it will join back the
" current line with the previous one. The latter works well with :setl fo+=l
" to prevent repeatedly breaking the line.
inoremap <expr> <C-G><C-G> (&textwidth == 0) ? '<C-\><C-O>gww' : '<Esc>kJgi'

nnoremap <C-X> <nop>

nnoremap <expr> <C-X><C-F> exists(':FZF') ? ':FZF<CR>' : ':edit<Space><C-D>'
nnoremap <expr> <C-X>f exists(':FZF') ? ':FZF ' . fnameescape(expand('%:h')).'/<CR>' : ':edit<Space>' . fnameescape(expand('%:h')).'/<C-D>'
nnoremap <C-X>0 <C-W>c
nnoremap <C-X>1 <C-W>o
nnoremap <C-X>2 <C-W>s
nnoremap <C-X>3 <C-W>v
nnoremap <C-X>o <C-W>w
nnoremap <C-X><C-B> :ls<CR>
nnoremap <C-X>b :buffer<Space><C-D>
" nnoremap <C-X>f :set textwidth=
nnoremap <C-X>c :confirm qall<CR>

function! s:BrowseNewTab(progname)
  tabnew
  set bt=nofile
  setl nonumber
  exec "0r !fetch-page " . a:progname
  let b:url = @+
  1
  " If the buffer is the output of lynx -dump, then with the cursor on a
  " number, the following will jump to the link reference with that number.
  nnoremap <buffer><expr> <C-]> 'm'':$?^ \+' . expand("<cword>") . '\.?<CR>W'
endfunction
command! BrowseNewTab :call <SID>BrowseNewTab("wget")
command! BrowseNewTabCurl :call <SID>BrowseNewTab("curl")

inoremap <expr> <C-C> pumvisible() ? '<C-E>' : '<C-C>'

" This block follows three principles:
"   (1) Any text editor that breaks CUA bindings for copy/cut/paste is broken.
"   (2) Any text editor that introduces security problems from pasting text
"       that was copied from a web browser is broken.
"   (3) Any text editor that, when the cursor is between two quote marks,
"       pastes text *outside* of those quote marks, is broken.
" Note that Vim breaks all three principles by default, whereas gedit breaks
" none.
if has('clipboard')
  " Destructively remove blank lines from both ends of a register and set it
  " to be characterwise. This is intended especially for pasting from the
  " quoteplus register; in almost all cases, pasting from the clipboard means
  " pasting from a different application that doesn't have any conception of
  " linewise or blockwise registers, so unconditional characterwise pasting is
  " more intuitive.
  function! s:MakeCharacterwise(reg)
    " In older versions of Vim, the call to setreg() at the end on the list
    " reg_cont crashes Vim with 'Caught deadly signal ABRT'. Git bisect
    " reveals that this is fixed with Vim 7.4 patch 513, 'Crash because
    " reference count is wrong for list returned by getreg().' Searching the
    " Git log reveals that there have been several bugs related to
    " setreg()/getreg(). The following conditional *should* fix things, but if
    " you are really worried, it's best to upgrade Vim or use the mapping from
    " $VIMRUNTIME/mswin.vim and $VIMRUNTIME/autoload/paste.vim.
    if has('nvim') || v:version > 704 || (v:version == 704 && has("patch513"))
      let reg_cont = getreg(a:reg, 1, 1)

      " Remove empty lines at the beginning and end of the register
      while (!empty(reg_cont)) && (reg_cont[-1] ==# '')
        call remove(reg_cont, -1)
      endwhile
      while (!empty(reg_cont)) && (reg_cont[0] ==# '')
        call remove(reg_cont, 0)
      endwhile
    else
      let reg_cont = getreg(a:reg)

      " Same idea as above; remove empty lines from the beginning and end.
      " Note that because the result of calling getreg() is not a list in this
      " case, any NULLs will be converted to newlines.
      while char2nr(strpart(reg_cont, len(reg_cont)-1)) == 10
        let reg_cont = strpart(reg_cont, 0, len(reg_cont)-1)
      endwhile
      while char2nr(strpart(reg_cont, 0, 1)) == 10
        let reg_cont = strpart(reg_cont, 1)
      endwhile
    endif

    call setreg(a:reg, reg_cont, 'c')
  endfunction

  vnoremap <silent> <C-X> "+x:<C-U>call <SID>MakeCharacterwise('+')<CR>
  vnoremap <silent> <C-C> "+y:<C-U>call <SID>MakeCharacterwise('+')<CR>

  nnoremap <silent> <C-V> :<C-U>call <SID>MakeCharacterwise('+')<CR>"+gP
  cnoremap <C-V> <C-R><C-R>+
  inoremap <silent> <C-V> <C-G>u<C-\><C-O>:<C-U>call <SID>MakeCharacterwise('+')<CR><C-R><C-O>+
  vnoremap <silent> <C-V> "-y:<C-U>call <SID>MakeCharacterwise('+')<CR>gv"+gp
  inoremap <silent> <C-G><C-V> <C-G>u<C-\><C-O>:<C-U>call <SID>MakeCharacterwise('+')<CR><C-R><C-R>+
endif

nnoremap <silent> Q Vip:!pdftextfmt<CR>gqq
vnoremap <silent> Q :!pdftextfmt<CR>gqq

" From Tim Pope:
" <https://github.com/tpope/tpope/blob/c743f64380910041de605546149b0575ed0538ce/.vimrc#L271>
nmap <silent> s :if &previewwindow<Bar>pclose<Bar>elseif exists(':Gstatus')<Bar>exe 'Gstatus'<Bar>else<Bar>ls<Bar>endif<CR>

" The 'else' case here is just :DiffOrig
nnoremap <silent> S :if exists(':Git')<Bar>update<Bar>exe 'silent !clear'<Bar>exe 'Git diff ' . shellescape(expand("%:p"))<Bar>else<Bar>vert new<Bar>set buftype=nofile<Bar>read ++edit #<Bar>0d_<Bar>diffthis<Bar>wincmd p<Bar>diffthis<Bar>endif<CR>

nnoremap <silent> <C-X><C-S> :if exists(':Gwrite')<Bar>exe 'Gwrite'<Bar>exe 'Gcommit'<Bar>else<Bar>write<Bar>endif<CR>

vnoremap K <nop>

iabbrev ADd Add
iabbrev REmove Remove

autocmd FileType markdown setlocal textwidth=79

let g:surround_{char2nr('q')} = "“\r”"
let g:surround_{char2nr('Q')} = "‘\r’"

xnoremap iq :<C-U>normal! T“vt”<CR>
xnoremap aq :<C-U>normal! F“vf”<CR>
onoremap iq :normal viq<CR>
onoremap aq :normal vaq<CR>

" I want to use my own <C-X> in visual mode, so disable mappings for
" speeddating and then define them myself. Without setting this variable,
" speeddating seems to override my mapping.
let g:speeddating_no_mappings = 1
nmap  <C-A>     <Plug>SpeedDatingUp
nmap  <C-S>     <Plug>SpeedDatingDown
xmap  <C-A>     <Plug>SpeedDatingUp
xmap  <C-S>     <Plug>SpeedDatingDown
nmap d<C-A>     <Plug>SpeedDatingNowUTC
nmap d<C-S>     <Plug>SpeedDatingNowLocal

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

let g:jedi#documentation_command = "<leader>K"
