PS1='[\u:\W]> '
alias {upgrade,update}='sudo aptitude update && sudo aptitude -y upgrade'
alias ls='ls --color=auto'
export HISTCONTROL=ignoreboth:erasedups
export HISTSIZE=10000
export PATH=$HOME/.cabal/bin:$PATH

alias deploy='git push origin master && git push bitbucket master && python3 generator/generator.py --commit_ps && rsync --delete --exclude=_archive/ -r _site/ carbon:/var/www/issarice.com/public_html && rsync -r _site/ carbon:/var/www/issarice.com/public_html/_archive/`git rev-parse --verify HEAD`'
alias newpage='cd /home/issa/projects/issarice.com && ./static/newpage.sh'
alias newlink='cd /home/issa/projects/issarice.com && ./static/newlink.py'
