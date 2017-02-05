# Issa Rice’s dotfiles

This repository stores configuration files for some common command-line
programs. This repository supersedes previous program-specific
configuration repositories like my old [Vim][vim-repo] and
[Neovim][neovim-repo] repositories.

All paths are relative to `$HOME`.

To install, edit `install.sh` and `debian_software.py` (these are the main
scripts; the rest are config files) then run `./install.sh` (which calls
`debian_software.py`).

## TODO

- clarify license for each file (they're all *free* but I often work off
  the example provided by the software, and each software has a
  different license)
- add mutt
- irssi
- bitlbee
- newsbeuter: change cache size so all content can be backed up
  (actually I now think this might be done by default). Also figure out
  how to sync cache.db.
- bash aliases, etc. bash config
- set up python to work (e.g. getting pip to work...), as well as other development environments (e.g. haskell)
- emacs: emacs config for Japanese input (see [my page on
  this](http://issarice.com/japanese-input-on-the-command-line-framebuffer))
- On LMDE `less` searches aren't highlighting for some reason under
  tmux. It's not a big deal, but I should fix it at some point. I tried
  [this](http://stackoverflow.com/questions/10535432/tmux-man-page-search-highlighting/10563271#10563271)
  but it didn't work.
- figure out how to make key combinations like CTRL-Up and SHIFT-Up work under
  various conditions (urxvt+tmux in emacs, urxvt in emacs, urxvt+tmux in Vim,
  and so on).

vim todo:

- learn how to move to fancy characters like `“` quickly; e.g. doing `f"` works to move to quotes, but moving to `“` would require `f<C-k>"6` which is cumbersome to type
- learn to navigate japanese documents better
- figure out how to configure YCM; see [here](https://www.reddit.com/r/vim/comments/2ez19c/an_allaround_solution_for_ycms_ycm_extra_confpy/) and [here](https://github.com/vheon/dotvim/blob/5321347027c21e4c22dc6fcea4cc315052ed25f1/ycm.py) for some example config files.

# History

I think parts of my vimrc date back to sometime in 2010 or 2011 when I really
first started using Vim (my [really old Vim repo][vim_old] was created on July
7, 2012 and  [old Vim repo][vim-repo] was created in May 2014; on a Git
repository that I don't think I ever mirrored to a public place, I have a
`.vimrc` with commit timestamp `Mon Aug 22 20:36:58 2011 -0700`, which *might*
be the first).
Something like `debian_packages.py` was also originally written fairly early
on, but I don't remember when that was.
This repository itself began in August 2015.

# License

Ideally I want to make everything in this repository available under the
public domain (specifically released to the public domain according to
the CC0), but some of the files here are forks of default config
files, which are usually GPL or BSD.
I'll try to keep track of licenses for each file here.

- `.vimrc`: Same license as Vim.

[vim-repo]: https://github.com/riceissa/vim
[neovim-repo]: https://github.com/riceissa/neovim
[vim_old]: https://github.com/riceissa/vim-old "Originally at riceissa/.vim."
