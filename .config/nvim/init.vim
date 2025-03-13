set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
if filereadable(expand('~/.vimrc'))
  source ~/.vimrc
endif
