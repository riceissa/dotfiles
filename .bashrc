if [ $(uname -o) != "Msys" ]; then
    PS1='\w\$ '
fi

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

export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=10000
# Unset so that the history is not truncated when initialized from the history
# file.
export HISTFILESIZE=

PATH="$HOME/.cabal/bin:$PATH"
# For some reason this one is already in my path, so comment out for now
# PATH="$HOME/.local/bin:$PATH"
PATH="$HOME/projects/dotfiles/.local/bin:$PATH"
PATH="$HOME/projects/pandoc-wikilinks-filter:$PATH"
export PATH

[[ $TMUX = "" ]] && export TERM='xterm-256color'

alias ls='ls --color=auto'
alias fd='fdfind'

# Emacs shell mode doesn't like the fzf bindings, so don't source
# fzf settings if inside Emacs. fzf doesn't work anyway, since shell
# mode cannot work well with shell escape sequences.
if [ $(uname -o) != "Msys" ]; then
    if [ -z "$INSIDE_EMACS" ]; then
        [ -f ~/.fzf.bash ] && source ~/.fzf.bash
    fi
fi
alias em='emacsclient -t'

if [ $(uname -o) != "Msys" ]; then
    alias vim=nvim
fi
alias svim='vim -Nu ~/sensible.vim'

if [ $(uname -o) == "Msys" ]; then
    export EDITOR=vim
    export VISUAL=vim
else
    export EDITOR=nvim
    export VISUAL=nvim
fi

# Enable CTRL-S in terminal
stty -ixon

# Filter clipboard: clean linebreaks in copied text
if [ $(uname -o) != "Msys" ]; then
    alias fclip='xclip -selection c -o | pdftextfmt | xclip -selection c'
fi

# The following works in Ubuntu when installing fzf using apt (which I prefer,
# since that way I don't have to remember to manually upgrade fzf).
[ -f /usr/share/doc/fzf/examples/key-bindings.bash ] && source /usr/share/doc/fzf/examples/key-bindings.bash

# Set ag/ripgrep as the default source for fzf if it exists
if [ $(uname -o) != "Msys" ]; then
    command -v ag >/dev/null 2>&1 && export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
    command -v rg >/dev/null 2>&1 && export FZF_DEFAULT_COMMAND='rg --hidden --files'
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    export FZF_CTRL_R_OPTS='-e'
fi

# For whatever reason, the WSL version of Ubuntu doesn't automatically start
# ssh-agent so the following will do that at startup.
if [ -n "$WSL_DISTRO_NAME" ]; then
    # The code below comes from https://docs.github.com/en/authentication/connecting-to-github-with-ssh/working-with-ssh-key-passphrases#auto-launching-ssh-agent-on-git-for-windows
    env=~/.ssh/agent.env

    agent_load_env () { test -f "$env" && . "$env" >| /dev/null ; }

    agent_start () {
        (umask 077; ssh-agent >| "$env")
        . "$env" >| /dev/null ; }

    agent_load_env

    # agent_run_state: 0=agent running w/ key; 1=agent w/o key; 2= agent not running
    agent_run_state=$(ssh-add -l >| /dev/null 2>&1; echo $?)

    if [ ! "$SSH_AUTH_SOCK" ] || [ $agent_run_state = 2 ]; then
        agent_start
        ssh-add
    elif [ "$SSH_AUTH_SOCK" ] && [ $agent_run_state = 1 ]; then
        ssh-add
    fi

    unset env
    # End of code from GitHub documentation
fi

# For whatever reason, the WSL version of Ubuntu doesn't automatically start
# MySQL at boot so the following makes sure this happens.
if [ -n "$WSL_DISTRO_NAME" ]; then
    wsl.exe -u root service mysql status > /dev/null || wsl.exe -u root service mysql start > /dev/null
fi
