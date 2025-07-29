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
