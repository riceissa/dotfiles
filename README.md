# Issa Rice’s dotfiles

This repository stores configuration files for some common command-line
programs. This repository supersedes previous program-specific
configuration repositories like my old [Vim][vim-repo] and
[Neovim][neovim-repo] repositories.

[vim-repo]: https://github.com/riceissa/vim
[neovim-repo]: https://github.com/riceissa/neovim

All paths are relative to `~/`.

To install, edit `install.sh` and `debian_software.py` (these are the main scripts; the rest are config files) then run `./install.sh`.

## To-do

- clarify license for each file (they're all *free* but I often work off
  the example provided by the software, and each software has a
  different license)
- add mutt
- irssi
- bitlbee
- git config
- ensure vim/neovim/gvim all work nicely everywhere
- newsbeuter: change cache size so all content can be backed up.
- elinks: merge keys with VimFX? I'm still not sure how much application-specific keybinding I'm willing to allow. In general I want everything to work like Vim, but many programs (e.g. elinks) don't support key combinations (except with hold-down combination with ctrl, alt, etc.).
- w3m
- terminator/gnome terminal configs (esp. getting solarized to work)
    - on gnome terminal I just set the background color to `#FDF6E3` and the text color to `#586E75`; I use the Tango colors (they work well enough with the solarized base) and Source Code Pro size 10
    - newer versions of terminator have solarized built in; however, the colors don't work very well with vim (not sure why) so I stick to solarized base and Tango colors (just like in gnome terminal)
- bash aliases, etc. bash config
- set up python to work (e.g. getting pip to work...)

vim todo:

- learn how to move to fancy characters like `“` quickly; e.g. doing `f"` works to move to quotes, but moving to `“` would require `f<C-k>"6` which is cumbersome to type
- learn to navigate japanese documents better
- Description: The intention here is to produce a sane .vimrc that makes both Vim and
Neovim act in the same way. As such, where Neovim changes a default option
from that in Vim, this is set explicitly here. For a more minimal
configuration that is still very good, there is sensible.vim:
<https://github.com/tpope/vim-sensible>


# License

Ideally I want to make everything in this repository available under the public domain (specifically released to the public domain according to the Unlicense), but some of the files here are forks of default config files, which are usually GPL or BSD.
I'll try to keep track of licenses for each file here.

- `.vimrc`: Unlicense
