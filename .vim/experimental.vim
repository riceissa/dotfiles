" Fix common typos where one character is stuck to the beginning of the next
" word or the end of the last word.
inoremap <C-G>h <C-G>u<Esc>BxgEpgi
inoremap <C-G>l <C-G>u<Esc>gExpgi

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
  digraph v\| 8595
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
cnoremap <C-G><C-K> <C-\>eCmdlineKillToEnd()<CR>
function! CmdlineKillToEnd()
  let pos = getcmdpos()
  if pos == 1
    " Vim's string indexing is messed up so I think we need a special case
    " here. cmd[0 : -1] would select the whole string.
    return ""
  else
    let cmd = getcmdline()
    " subtract two because right index is inclusive and because getcmdpos()
    " starts at 1
    return cmd[0 : pos-2]
  endif
endfunction
cnoremap <C-X><C-F> <C-F>
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
nmap  <C-S>     <Plug>SpeedDatingDown
xmap  <C-A>     <Plug>SpeedDatingUp
xmap  <C-S>     <Plug>SpeedDatingDown
nmap d<C-A>     <Plug>SpeedDatingNowUTC
nmap d<C-S>     <Plug>SpeedDatingNowLocal

inoremap <M-q> <C-\><C-O>gwip
nnoremap <M-q> gwip
vnoremap <M-q> gw
if !has("gui_running") && !has('nvim')
  silent! exe "set <F36>=\<Esc>q"
  map! <F36> <M-q>
  map <F36> <M-q>
endif
