" Use vim-plug to manage Vim plugins. See https://github.com/junegunn/vim-plug
" for full instructions.
"
" Once all Vim config files are in the right places, just do :PlugInstall in
" Vim to install the plugins. The exception is YouCompleteMe, which probably "
" needs to be compiled; see below for more.

set nocompatible              " be iMproved, required?

call plug#begin('~/.vim/plugged')
Plug 'SirVer/ultisnips'
Plug 'altercation/vim-colors-solarized' " only for gvim
Plug 'ctrlpvim/ctrlp.vim'
Plug 'honza/vim-snippets'
Plug 'nelstrom/vim-visual-star-search'
Plug 'riceissa/vim-more-toggling'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-characterize'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-unimpaired'

" YouCompleteMe
" -------------
" TODO: YCM might actually automatically install now that I'm using Ubuntu
" 16.04 and vim-plug instead of Ubuntu 14.04 and Vundle.
"
" This plugin doesn't seem to work with just :PluginInstall, so compile it as
" follows after calling :PluginInstall in Vim (this step is still necessary
" since we have to clone the YouCompleteMe repository)
"       cd ~/.vim/bundle/YouCompleteMe
"       ./install.py --clang-completer
Plug 'Valloric/YouCompleteMe', { 'for': ['python', 'java', 'c']}

" All of your Plugins must be added before the following line
call plug#end()

" UltiSnips
" ---------
let g:UltiSnipsSnippetDirectories=["UltiSnips", "UltiSnips-custom-snippets"]
let g:UltiSnipsExpandTrigger="<C-j>"
let g:UltiSnipsJumpForwardTrigger="<C-j>"
let g:UltiSnipsJumpBackwardTrigger="<C-k>"

" YouCompleteMe
" -------------
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

let g:pandoc#syntax#conceal#use = 0

" Ignore files in .gitignore
let g:ctrlp_user_command = ['.git/', 'git --git-dir=%s/.git ls-files -oc --exclude-standard']
let g:ctrlp_map = '<c-k>'
