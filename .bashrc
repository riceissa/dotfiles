# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth:erasedups

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=100000
# Unset so that the history is not truncated when initialized from the history
# file.
HISTFILESIZE=

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# ^========== ABOVE: UBUNTU'S DEFAULT BASHRC, WITH SOME MODIFICATIONS ==========^
# v========== BELOW: START OF MY OWN BASH CUSTOMIZATIONS ==========v


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

[ -f "/home/issa/.ghcup/env" ] && source "/home/issa/.ghcup/env" # ghcup-env
