PS1='\h:\W\$ '

# Modified from <https://www.jefftk.com/p/you-should-be-logging-shell-history>
promptFunc() {
    # right before prompting for the next command, save the previous
    # command in a file.
    echo "$(date -Iseconds) $(hostname) $PWD $(history 1)" \
        >> ~/.full_history
}
PROMPT_COMMAND=promptFunc

export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=10000
# Unset so that the history is not truncated when initialized from the history
# file.
export HISTFILESIZE=

export EDITOR="vim"
export GOPATH=/home/issa/go/packages
PLAN9=/home/issa/projects/plan9port export PLAN9

PATH="$PATH:$GOPATH/bin"
PATH="$PATH:$PLAN9/bin"
PATH="$PATH:/usr/games"
PATH="$HOME/.local/bin:$PATH"
PATH="$HOME/.cabal/bin:$PATH"
export PATH

[[ $TMUX = "" ]] && export TERM='xterm-256color'

alias ls='ls --color=auto'
alias mupdf='tmux-mupdf'

stty -ixon

# Set ag as the default source for fzf if it exists
command -v ag >/dev/null 2>&1 && export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_R_OPTS='-e'
