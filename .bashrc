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
# Unset so that the history is not truncated when initialized from the history
# file.
export HISTFILESIZE=
PLAN9=/home/issa/projects/plan9port export PLAN9
PATH=$PATH:$PLAN9/bin export PATH
PATH=$PATH:/usr/games export PATH
# PATH="$HOME/.local/bin:$PATH"
PATH="$PATH:$GOPATH/bin"
export PATH="$HOME/.cabal/bin:$PATH"
[[ $TMUX = "" ]] && export TERM='xterm-256color'

alias swapswap='sudo swapoff -a && sudo swapon -a'
alias mupdf='mupdf -C FDF6E3'

stty -ixon

# Set ag as the default source for fzf
export FZF_DEFAULT_COMMAND='ag -g ""'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_R_OPTS='-e'

export EDITOR="vim"
export GOPATH=/home/issa/go/packages
