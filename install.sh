#!/bin/bash
set -e

# Vim and Neovim
# --------------
git clone git://github.com/riceissa/dotfiles.git
mkdir -p ~/.vim/bundle
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
#git clone git@github.com:riceissa/dotfiles.git
cd dotfiles
ln -s "$(pwd)/.vimrc" ~/.vimrc
ln -s "$(pwd)/.gvimrc" ~/.gvimrc
ln -s ~/.vimrc ~/.nvimrc
ln -s ~/.vim ~/.nvim
ln -s "$(pwd)/.vim/plugins.vim" ~/.vim/plugins.vim
ln -s "$(pwd)/.vim/mswin_extract.vim" ~/.vim/mswin_extract.vim
ln -s "$(pwd)/.tmux.conf" ~/.tmux.conf
# For tmux
echo 'source ~/.bashrc' >> ~/.bash_profile
mkdir -p ~/.moc/themes
ln -s "$(pwd)/.moc/config" ~/.moc/config
ln -s "$(pwd)/.moc/themes/my_theme" ~/.moc/themes/my_theme
# for UltiSnips
pip install --user neovim
