PS1='\h:\W\$ '

# Modified from <https://www.jefftk.com/p/you-should-be-logging-shell-history>
promptFunc() {
    # right before prompting for the next command, save the previous
    # command in a file.
    echo "$(date -Iseconds) $(hostname) $PWD $(history 1)" \
        >> ~/.full_history
}
PROMPT_COMMAND=promptFunc

alias ls='ls --color=auto'
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=10000
export PATH=$HOME/.cabal/bin:$PATH
[[ $TMUX = "" ]] && export TERM='xterm-256color'

alias swapswap='sudo swapoff -a && sudo swapon -a'
alias mupdf='mupdf -C FDF6E3'

stty -ixon
