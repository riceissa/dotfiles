#!/bin/bash

git clone https://github.com/riceissa/dotfiles.git
# To switch to SSH later do:
#     git remote remove origin
#     git remote add origin git@github.com:riceissa/dotfiles.git
cd dotfiles

# Install software
python debian_packages.py

ln -s "$(pwd)/.bashrc" ~/.bashrc
# ln -s "$(pwd)/.zshrc" ~/.zshrc

# Get vim-plug to manage plugins
curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

ln -s "$(pwd)/.vimrc" ~/.vimrc
ln -s "$(pwd)/.gvimrc" ~/.gvimrc
ln -s "$(pwd)/.vim/UltiSnips-custom-snippets" ~/.vim/UltiSnips-custom-snippets
ln -s "$(pwd)/.ycm_extra_conf.py" ~/.ycm_extra_conf.py

# for UltiSnips
pip install --user neovim

# Neovim support
mkdir -p ${XDG_CONFIG_HOME:=$HOME/.config}
ln -s ~/.vim $XDG_CONFIG_HOME/nvim
ln -s ~/.vimrc $XDG_CONFIG_HOME/nvim/init.vim

ln -s "$(pwd)/.tmux.conf" ~/.tmux.conf
# tmux doesn't read from .bashrc so copy contents to .bash_profile
echo 'source ~/.bashrc' >> ~/.bash_profile

mkdir -p ~/.moc/themes
ln -svf "$(pwd)/.moc/config" ~/.moc/config
ln -svf "$(pwd)/.moc/my_keymap" ~/.moc/my_keymap
ln -svf "$(pwd)/.moc/themes/my_theme" ~/.moc/themes/my_theme

ln -svf "$(pwd)/.gitconfig" ~/.gitconfig
ln -svf "$(pwd)/.gitignore_global" ~/.gitignore_global

mkdir -p ~/.newsbeuter
ln -svf "$(pwd)/.newsbeuter/config" ~/.newsbeuter/config

mv -v ~/.emacs ~/.emacs.$(date -Idate).bak 2> /dev/null
ln -svf "$(pwd)/.emacs" ~/.emacs
