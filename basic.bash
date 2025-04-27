# This file is for bash configuration that I would like to use on every
# machine.

# Modified from <https://www.jefftk.com/p/you-should-be-logging-shell-history>
promptFunc() {
    # right before prompting for the next command, save the previous
    # command in a file.
    echo "$(date -Iseconds) $(hostname) $PWD $(history 1)" \
        >> ~/.full_history
}
PROMPT_COMMAND=promptFunc

# Pressing CTRL-D is easiest way to exit the shell, but it does not leave a
# line in the bash history.  Since the promptfunc history logging above logs
# the most recently run command, this means that whatever "real" command one
# ran last will get logged again and again every time one opens a new shell
# window. Try this: with the promptfunc above enabled, type "less +F
# ~/.full_history" to watch the file, then open some new shell windows. You
# will see that whatever terminal command you happened to last run (from a
# shell session that has already ended) will keep getting logged. To prevent
# this sort of garbage logging, one solution is to standardize on the final
# command one runs before exiting the shell; this way, anytime one opens a new
# shell window, the standard command will be logged. And because it's a
# standard command, it will not be confusing to look at the full history: the
# standard command just means a new shell window was opened. One natural choice
# for the standard final command is "exit"; we want to just close the
# commandline after all. However, typing "exit" each time to close the
# commandline is a little annoying. The following alias makes exiting a little
# less cumbersome. It also never exits when there are jobs, which is quite
# useful on its own.
alias e='if [[ $(jobs) ]]; then jobs; else exit; fi'

# Tmux will start login shells by default. I don't know if that's really a good
# thing to have, but I don't want to go around changing that behavior of Tmux
# without having deep understanding. But a side-effect of Tmux starting login
# shells is that Tmux will source the bashrc again (even though the base layer
# of terminal emulator has already sourced it once), so $PATH modifications
# will be repeated, leading to duplicate directories in $PATH. The following,
# taken from https://superuser.com/a/39995 , will check to make sure a given
# directory is not already in $PATH before adding it.
path_prepend() {
    if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
        PATH="$1${PATH:+":$PATH"}"
    fi
}

# On Ubuntu, the ~/.profile file by default also prepends ~/.local/bin to the
# $PATH, so make sure to comment that file out, or else ~/.local/bin will be
# duplicated in $PATH *three* times (the first time it will be added correctly
# by this ~/.bashrc, and then it will be added incorrectly by ~/.profile upon
# opening the base terminal, and then it will be added a third time again by
# ~/.profile by tmux, which by default starts login shells).
path_prepend "$HOME/.local/bin"
export PATH

[[ $TMUX = "" ]] && export TERM='xterm-256color'

# Enable CTRL-S in terminal
stty -ixon

# The following works in Ubuntu when installing fzf using apt (which I prefer,
# since that way I don't have to remember to manually upgrade fzf).
# See `apt show fzf` (which just points to /usr/share/doc/fzf/README.Debian)
# for more information.
[ -f /usr/share/doc/fzf/examples/key-bindings.bash ] && source /usr/share/doc/fzf/examples/key-bindings.bash
