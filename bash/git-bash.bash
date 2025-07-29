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

alias python3='winpty py'
alias python='winpty py'

alias git-backup='git add . && git commit -m "snapshot" && git push'

# fzf doesn't seem to work on gitbash?
# [ -f ~/.fzf.bash ] && source ~/.fzf.bash
[ -f /c/Users/Issa/projects/dotfiles/basic.bash ] && source /c/Users/Issa/projects/dotfiles/basic.bash

path_prepend "/c/Users/Issa/AppData/Local/Programs/Python/Python313/Scripts/"

export EDITOR=vim
export VISUAL=vim
