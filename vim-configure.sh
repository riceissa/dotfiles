#!/bin/bash

# See https://github.com/Valloric/YouCompleteMe/wiki/Building-Vim-from-source.
# This is meant to work with Ubuntu 16.04, and was used to compile Vim 8.0.

./configure --with-features=huge \
  --enable-multibyte \
  --enable-rubyinterp \
  --enable-python3interp \
  --with-python3-config-dir=/usr/lib/python3.5/config-3.5m-x86_64-linux-gnu \
  --enable-perlinterp \
  --enable-luainterp \
  --enable-gui=gtk2 --enable-cscope --prefix=/usr
