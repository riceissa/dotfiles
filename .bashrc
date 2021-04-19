PS1='\h:\w\$ '

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

PATH="$HOME/.cabal/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
export PATH

[[ $TMUX = "" ]] && export TERM='xterm-256color'

alias ls='ls --color=auto'
alias fd='fdfind'

# CTRL-D is easiest to type, but does not leave a line in the bash history,
# leading to the full history logging via promptfunc containing unexpected
# garbage. This alias makes exiting a little less cumbersome. It also never
# exits when there are jobs.
alias e='if [[ $(jobs) ]]; then jobs; else exit; fi'

# Emacs shell mode doesn't like the fzf bindings, so don't source
# fzf settings if inside Emacs. fzf doesn't work anyway, since shell
# mode cannot work well with shell escape sequences.
if [ -z "$INSIDE_EMACS" ]; then
    [ -f ~/.fzf.bash ] && source ~/.fzf.bash
fi
alias em='emacsclient -t'

alias vim=nvim
alias svim='vim -Nu ~/sensible.vim'

export EDITOR=nvim
export VISUAL=nvim

# Enable CTRL-S in terminal
stty -ixon

# Set ag as the default source for fzf if it exists
command -v ag >/dev/null 2>&1 && export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_R_OPTS='-e'
