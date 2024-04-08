#!/usr/bin/env bash

nvim -c 'source dump_settings.vim|write! out-nvim.txt|quit'

/usr/bin/vim -c 'source dump_settings.vim|write! out-vim.txt|quit'
