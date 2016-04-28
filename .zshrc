# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=1000000
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/issa/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
autoload -U select-word-style
select-word-style bash

[[ $TMUX = "" ]] && export TERM='xterm-256color'
PS1='%m:%c%# '
alias ls='ls --color=auto'
alias swapswap='sudo swapoff -a && sudo swapon -a'
stty -ixon
