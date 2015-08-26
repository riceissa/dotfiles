#!/bin/bash
set -e

#link to dotfiles using ln -s "$(pwd)/file" ~/.vimrc -- this is just an example

git clone git://github.com/riceissa/dotfiles.git
#git clone git@github.com:riceissa/dotfiles.git
cd dotfiles
ln -s "$(pwd)/.vimrc" ~/.vimrc
ln -s "$(pwd)/.gvimrc" ~/.gvimrc

 cd ~/.vim && git submodule update --init --recursive
