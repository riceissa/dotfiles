# Issa Rice’s dotfiles

This repository stores configuration files for some common command-line
programs. This repository supersedes previous program-specific
configuration repositories like my old [Vim][vim-repo] and
[Neovim][neovim-repo] repositories.

All paths are relative to `~/`.

To install, edit `install.sh` and `debian_software.py` (these are the main scripts; the rest are config files) then run `./install.sh`.

## To-do

- clarify license for each file (they're all *free* but I often work off
  the example provided by the software, and each software has a
  different license)
- add mutt
- irssi
- bitlbee
- ensure vim/neovim/gvim all work nicely everywhere
- newsbeuter: change cache size so all content can be backed up
  (actually I now think this might be done by default). Also figure out
  how to sync cache.db.
- elinks: merge keys with VimFX? I'm still not sure how much
  application-specific keybinding I'm willing to allow. In general I
  want everything to work like Vim, but many programs (e.g. elinks)
  don't support key combinations (except with hold-down combination with
  ctrl, alt, etc.).
- w3m, because elinks can't read Japanese characters unless they're in
  UTF-8! (should follow closely as possible the keybindings of elinks,
  in order to not confuse the user; actually, use the same keybindings
  for "newsbeuter", "elinks", and "w3m")
- terminator/gnome terminal configs (esp. getting solarized to work)
    - on gnome terminal I just set the background color to `#FDF6E3` and
      the text color to `#586E75`; I use the Tango colors (they work
      well enough with the solarized base) and Source Code Pro size 10
    - newer versions of terminator have solarized built in; however, the
      colors don't work very well with vim (not sure why) so I stick to
      solarized base and Tango colors (just like in gnome terminal)
- bash aliases, etc. bash config
- set up python to work (e.g. getting pip to work...)
- emacs: emacs config for Japanese input (see [my page on
  this](http://issarice.com/japanese-input-on-the-command-line-framebuffer))
- framebuffer stuff, like fbterm command to change the font (probably
  include in .bashrc)
- document firefox plugins: adblock plus, vimfx, noscript, duckduckgo.
  maybe also epub plugin.
- On LMDE `less` searches aren't highlighting for some reason under
  tmux. It's not a big deal, but I should fix it at some point. I tried
  [this](http://stackoverflow.com/questions/10535432/tmux-man-page-search-highlighting/10563271#10563271)
  but it didn't work.

vim todo:

- learn how to move to fancy characters like `“` quickly; e.g. doing `f"` works to move to quotes, but moving to `“` would require `f<C-k>"6` which is cumbersome to type
- learn to navigate japanese documents better
- Description: The intention here is to produce a sane .vimrc that makes
  both Vim and Neovim act in the same way. As such, where Neovim changes
  a default option from that in Vim, this is set explicitly here. For a
  more minimal configuration that is still very good, there is
  sensible.vim: <https://github.com/tpope/vim-sensible>
- Another way to describe my intention with my .vimrc: it's basically
  sensible.vim + Neovim's defaults, with a few tweaks to make it easier
  for my (idiosyncratically).
- figure out how to configure YCM; see [here](https://www.reddit.com/r/vim/comments/2ez19c/an_allaround_solution_for_ycms_ycm_extra_confpy/) and [here](https://github.com/vheon/dotvim/blob/5321347027c21e4c22dc6fcea4cc315052ed25f1/ycm.py) for some example config files.

# History

I think parts of my vimrc date back to sometime in 2010 or 2011 when I really
first started using Vim (my [really old Vim repo][vim_old] was created on July
7, 2012 and  [old Vim repo][vim-repo] was created in May 2014; on a Git
repository that I don't think I ever mirrored to a public place, I have a
`.vimrc` with commit datestamp `Mon Aug 22 20:36:58 2011 -0700`, which *might*
be the first).
Something like `debian_packages.py` was also originally written fairly early
on, but I don't remember when that was.
This repository itself began in August 2015.

# License

Ideally I want to make everything in this repository available under the
public domain (specifically released to the public domain according to
the Unlicense), but some of the files here are forks of default config
files, which are usually GPL or BSD.
I'll try to keep track of licenses for each file here.

- `.vimrc`: Same license as Vim.

[vim-repo]: https://github.com/riceissa/vim
[neovim-repo]: https://github.com/riceissa/neovim
[vim_old]: https://github.com/riceissa/vim-old
