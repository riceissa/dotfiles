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
- elinks
- w3m
- terminator/gnome terminal configs (esp. getting solarized to work)
    - on gnome terminal I just set the background color to `#FDF6E3` and the text color to `#586E75`; I use the Tango colors (they work well enough with the solarized base) and Source Code Pro size 10
    - newer versions of terminator have solarized built in; however, the colors don't work very well with vim (not sure why) so I stick to solarized base and Tango colors (just like in gnome terminal)
- bash aliases, etc. bash config
- set up python to work (e.g. getting pip to work...)

vim todo:

- learn how to move to fancy characters like `“` quickly; e.g. doing `f"` works to move to quotes, but moving to `“` would require `f<C-k>"6` which is cumbersome to type
- learn to navigate japanese documents better
