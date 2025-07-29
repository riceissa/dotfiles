if [ $(uname -o) != "Msys" ]; then
    PS1='\w\$ '
fi

path_prepend "$HOME/.cabal/bin"
path_prepend "$HOME/projects/dotfiles/.local/bin"
path_prepend "$HOME/projects/pandoc-wikilinks-filter"
path_prepend "$HOME/.nimble/bin"
path_prepend "$HOME/go/bin"
path_prepend "/opt/nvim-linux-x86_64/bin"
export PATH

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

alias vim=nvim
export EDITOR=nvim
export VISUAL=nvim

# Set ag/ripgrep as the default source for fzf if it exists
# if [ $(uname -o) != "Msys" ]; then
#     command -v ag >/dev/null 2>&1 && export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
#     command -v rg >/dev/null 2>&1 && export FZF_DEFAULT_COMMAND='rg --hidden --files'
#     export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
#     export FZF_CTRL_R_OPTS='-e'
# fi

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
