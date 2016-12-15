#!/bin/bash
# First version written on or around December 4, 2016.
#
# This script is to be run periodically (e.g. each minute as a cron job) to
# keep Vim and Mutt cached and ever-present in memory. This way, it is very
# quick to split a new tmux window and write an email.
#
# For some reason, running 'vim -c quit' prints some escape sequences to
# stdout. Redirecting with 'vim -c quit &> /dev/null' works, but it takes over
# two seconds to run!
#
# Note: One side-effect of running this script periodically as a cron job is
# that it will probably run before you start tmux for doing actual work. This
# means that your own tmux session will be numbered "1" (rather than "0") and
# more annoyingly the default shell will be changed to sh. One workaround is
# to include the following line in ~/.tmux.conf:
#     set-option -g default-shell /bin/bash

# Start a new tmux session with a bash shell if none exists with the name
# "keep_vim_mutt"
tmux has-session -t keep_vim_mutt 2> /dev/null || tmux new-session -s keep_vim_mutt -d

tmux new-window -t keep_vim_mutt 'vim -c quit'
tmux new-window -t keep_vim_mutt 'mutt -F /dev/null -e "exec quit"'
# tmux new-window -t keep_vim_mutt 'sleep 30'

# sleep 1 # wait for prompt
# Quit the bash shell, which is window 0
# tmux send-keys -t keep_vim_mutt:0 'exit' Enter
