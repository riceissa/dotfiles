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

nnoremap <C-N> gj
nnoremap <C-P> gk
vnoremap <C-N> gj
vnoremap <C-P> gk

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

imap <C-G><C-V> <Plug>CuaccpIHardwrapPaste

nnoremap <silent> gQ Vip:!pdftextfmt<CR>:<C-R>=&textwidth>0?'normal! gqq':''<CR><CR>
vnoremap <silent> gQ :!pdftextfmt<CR>:<C-R>=&textwidth>0?'normal! gqq':''<CR><CR>
nnoremap <silent> Q gwip
vnoremap <silent> Q gw

vnoremap K <nop>

iabbrev ADd Add
iabbrev REmove Remove

autocmd FileType xdefaults setlocal commentstring=!%s
" From defaults.vim
" Put these in an autocmd group, so that you can revert them with:
" ":augroup vimStartup | au! | augroup END"
augroup vimStartup
  au!

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  autocmd BufReadPost *
    \ if line("'\"") >= 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

augroup END

let g:surround_{char2nr('q')} = "“\r”"
let g:surround_{char2nr('Q')} = "‘\r’"

" I want to use my own <C-X> in visual mode, so disable mappings for
" speeddating and then define them myself. Without setting this variable,
" speeddating seems to override my mapping.
let g:speeddating_no_mappings = 1
nmap  <C-A>     <Plug>SpeedDatingUp
nmap  <C-X>     <Plug>SpeedDatingDown
xmap  <C-A>     <Plug>SpeedDatingUp
xmap  <C-S>     <Plug>SpeedDatingDown
nmap d<C-A>     <Plug>SpeedDatingNowUTC
nmap d<C-X>     <Plug>SpeedDatingNowLocal
