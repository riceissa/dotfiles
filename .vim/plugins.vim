" Use Vundle to manage Vim plugins; see
" https://github.com/VundleVim/Vundle.vim for more
"
" Once all Vim config files are in the right places, just do :PluginInstall in
" Vim to install the plugins. The exception is YouCompleteMe, which probably
" needs to be compiled; see below for more.

set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'tpope/vim-fugitive'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'altercation/vim-colors-solarized'
Plugin 'ctrlpvim/ctrlp.vim'

" YouCompleteMe
" -------------
" This plugin doesn't seem to work with just :PluginInstall, so compile it as
" follows after calling :PluginInstall in Vim (this step is still necessary
" since we have to clone the YouCompleteMe repository)
"       cd ~/.vim/bundle/YouCompleteMe
"       ./install.py --clang-completer
"Plugin 'Valloric/YouCompleteMe'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required
" To ignore plugin indent changes, instead use:
"filetype plugin on

" UltiSnips
" ---------
let g:UltiSnipsSnippetDirectories=["UltiSnips", "UltiSnips-custom-snippets"]
let g:UltiSnipsExpandTrigger="<C-j>"
let g:UltiSnipsJumpForwardTrigger="<C-j>"
let g:UltiSnipsJumpBackwardTrigger="<C-k>"

" YouCompleteMe
" -------------
let g:ycm_filetype_blacklist = {
    \ 'unite' : 1,
    \ 'markdown' : 1,
    \ 'text': 1,
    \ 'notes' : 1,
    \ 'pdc' : 1,
    \ 'pandoc' : 1,
    \ 'mail' : 1,
    \ 'gitcommit': 1,
    \ 'tex': 1,
    \ 'html': 1,
    \}

let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'
let g:EclimCompletionMethod = 'omnifunc'

" Ignore files in .gitignore
let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']
