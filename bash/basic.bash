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
