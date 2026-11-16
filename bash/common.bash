# This file is for bash configuration that I would like to use on every
# machine.

log_history_func() {
    local cmd
    cmd=$(HISTTIMEFORMAT='' history 1 | sed 's/^ *[0-9]*[ *]*//')
    printf '%s\t%s\t%s\t%s\n' "$(date -Iseconds)" "$HOSTNAME" "$PWD" "$cmd" >> ~/.full_history
}
PS0='$(log_history_func)'

# Pressing CTRL-D is the easiest way to exit the shell, but it will close the
# shell even if there are programs that have been put into the background using
# CTRL-Z. The following alias will check to see if there are any programs in
# the background, and will only exit if there aren't; if there *are* programs
# in the background, it will print them all, and you can deal with them using
# fg.
alias e='if [[ $(jobs) ]]; then jobs; else exit; fi'

# Tmux will start login shells by default. I don't know if that's really a good
# thing to have, but I don't want to go around changing that behavior of Tmux
# without having deep understanding. But a side-effect of Tmux starting login
# shells is that Tmux will source the bashrc again (even though the base layer
# of terminal emulator has already sourced it once), so $PATH modifications
# will be repeated, leading to duplicate directories in $PATH. The following,
# taken from https://superuser.com/a/39995 , will check to make sure a given
# directory is not already in $PATH before adding it.
# Use this function for example like this:
#     path_prepend "$HOME/projects/dotfiles/.local/bin"
path_prepend() {
    if [ -d "$1" ] && [[ ":$PATH:" != *":$1:"* ]]; then
        PATH="$1${PATH:+":$PATH"}"
    fi
}
