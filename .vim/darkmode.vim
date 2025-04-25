" Normally you would query for the presence of darkmode on Windows by doing
"     reg.exe query "HKCU\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize" /v AppsUseLightTheme
" But we can't do the /v stuff inside of Git Bash, so we do nested matchstr()
" calls instead.
let s:windows_darkmode = executable('reg.exe') && matchstr(matchstr(system('reg.exe query "HKCU\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize"'), 'AppsUseLightTheme.*$'), '0x[0-9]') ==# '0x0'
let s:darkmode = 0
if s:windows_darkmode
  let s:darkmode = 1
endif

" The following is more complicated than I would like, but I like to switch
" between light and dark depending on time of day, and some colorschemes are
" only available in Neovim and some colorschemes look terrrible with
" termguicolors while others look way better with termguicolors, so I just
" have to go through each possibility and set stuff to the combination of
" settings that looks the best to my eyes. Otherwise I will be stuck with
" horrible blinding light at night, or horrible visual artifacts that just
" don't look right, etc.
if s:darkmode
  set background=dark
  if has('nvim')
    set termguicolors
  else
    silent! colorscheme torte
  endif
else
  set background=light
  if has('nvim')
    set notermguicolors
    silent! colorscheme vim
  endif
endif
