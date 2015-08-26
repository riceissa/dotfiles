# Issa Rice's dotfiles

This repository stores configuration files for some common command-line
programs. This repository supersedes previous program-specific
configuration repositories like my old [Vim][vim-repo] and
[Neovim][neovim-repo] repositories.

[vim-repo]: https://github.com/riceissa/vim
[neovim-repo]: https://github.com/riceissa/neovim

All paths are relative to `~/`.

## todo

- add mutt
- irssi
- bitlbee
- git config
- ensure vim/neovim/gvim all work nicely everywhere
    - basically I don't really want to rely on too many plugins; they are a hassle to manage and also aren't practical to install on remote machines (whereas copying a single .vimrc is often possible)
    - also submodules make the main git repo *slow* (and also I don't really understand them very well) so it's best to just use a shell script to pull them in without entangling them with the main vim/dotfiles repo
    - YouCompleteMe
    - ultisnips (mostly useful for writing LaTeX documents, since I can never remember the syntax for e.g. tables)
    - fugitive? -- or maybe I should just learn tig
- elinks
- w3m
- terminator/gnome terminal configs (esp. getting solarized to work)
- bash aliases, etc. bash config
- set up python to work (e.g. getting pip to work...)
- add ctags to debian_software.py

vim todo:

- learn how to move to fancy characters like “ quickly; e.g. doing f" works to move to quotes, but moving to “ would require f<C-k>"6 which is cumbersome to type
- learn to navigate japanese documents better
- make tabs in Makefiles actual tabs:

    ```
    " Makefile options
    " ----------------
    augroup filetype_makefile
        autocmd!
        autocmd BufNewFile,BufRead Makefile setlocal noexpandtab
    augroup END
    ```

