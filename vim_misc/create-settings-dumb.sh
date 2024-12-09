#!/usr/bin/env bash

# nvim -Nu NONE -c 'source dump_settings.vim|write! out-nvim.txt|quit'
# /usr/bin/vim -Nu ~/sensible.vim -c 'source dump_settings.vim|write! out-vim.txt|quit'

nvim -c 'source dump_settings.vim|write! out-nvim.txt|quit'
/usr/bin/vim -c 'source dump_settings.vim|write! out-vim.txt|quit'
