#!/bin/bash
set -e

git clone git://github.com/riceissa/dotfiles.git
# The following might work better if I don't have SSH configured
#git clone git@github.com:riceissa/dotfiles.git
cd dotfiles

# Install software
python debian_packages.py

# Bash
# ----
ln -s "$(pwd)/.bashrc" ~/.bashrc

# Vim and Neovim
# --------------
# Get Vundle to manage Vim plugins
mkdir -p ~/.vim/bundle
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

# Mirror config files and dirs
ln -s "$(pwd)/.vimrc" ~/.vimrc
ln -s "$(pwd)/.gvimrc" ~/.gvimrc
ln -s "$(pwd)/.vim/mswin_extract.vim" ~/.vim/mswin_extract.vim
ln -s "$(pwd)/.vim/plugins.vim" ~/.vim/plugins.vim
ln -s "$(pwd)/.vim/UltiSnips-custom-snippets" ~/.vim/UltiSnips-custom-snippets
ln -s "$(pwd)/.ycm_extra_conf.py" ~/.ycm_extra_conf.py

# for UltiSnips
pip install --user neovim

# Neovim support
ln -s ~/.vimrc ~/.nvimrc
ln -s ~/.vim ~/.nvim

# tmux
# ----
ln -s "$(pwd)/.tmux.conf" ~/.tmux.conf
# tmux doesn't read from .bashrc so copy contents to .bash_profile
echo 'source ~/.bashrc' >> ~/.bash_profile

# MOC
# ---
mkdir -p ~/.moc/themes
ln -s "$(pwd)/.moc/config" ~/.moc/config
ln -s "$(pwd)/.moc/themes/my_theme" ~/.moc/themes/my_theme

# git
# ---
ln -s "$(pwd)/.gitconfig" ~/.gitconfig

# ELinks
# ------
mkdir ~/.elinks
echo elinks.conf >> ~/.elinks/elinks.conf
