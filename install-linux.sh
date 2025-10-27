#!/bin/bash

if [ -z "$1" ]; then
cat <<'EOF'
Usage:
    ./install-linux.sh [program...]
For more help:
    ./install-linux.sh --help
EOF
fi

while [ -n "$1" ]; do
    case "$1" in
    bash) install_bash=yes
        ;;
    editorconfig) install_editorconfig=yes
        ;;
    emacs) install_emacs=yes
        ;;
    gf) install_gf=yes
        ;;
    git) install_git=yes
        ;;
    kitty) install_kitty=yes
        ;;
    local_bin) install_local_bin=yes
        ;;
    neovim) install_neovim=yes
        ;;
    newsboat) install_newsboat=yes
        ;;
    proselint) install_proselint=yes
        ;;
    tmux) install_tmux=yes
        ;;
    vim) install_vim=yes
        ;;
    -h|--help) show_help=yes
        ;;
    esac
    shift
done

if [ -n "$show_help" ]; then
cat <<'EOF'
Install script for dotfiles.

./install-linux.sh [program...]
./install-linux.sh {-h|--help}

Supported programs: bash, editorconfig, emacs, gf, git, kitty, local_bin, neovim,
                    newsboat, proselint, tmux, vim

For instance to install dotfiles for Vim and tmux, run:

    ./install-linux.sh vim tmux
EOF
    exit
fi

if [ -n "$install_bash" ]; then
    if grep -q -e "^HISTSIZE" ~/.bashrc; then
        echo "HISTSIZE found; editing value to 1000000"
        sed -i 's/^HISTSIZE=.*/HISTSIZE=1000000/' ~/.bashrc
    else
        echo "HISTSIZE not found; adding HISTSIZE=1000000"
        echo "HISTSIZE=1000000" >> ~/.bashrc
    fi
    if grep -q -e "^HISTFILESIZE" ~/.bashrc; then
        echo "HISTFILESIZE found; editing value to 1000000"
        sed -i 's/^HISTFILESIZE=.*/HISTFILESIZE=1000000/' ~/.bashrc
    else
        echo "HISTFILESIZE not found; adding HISTFILESIZE=1000000"
        echo "HISTFILESIZE=1000000" >> ~/.bashrc
    fi
    if grep -q -e "^HISTCONTROL" ~/.bashrc; then
        echo "HISTCONTROL found; editing value to ignoreboth:erasedups"
        sed -i 's/^HISTCONTROL=.*/HISTCONTROL=ignoreboth:erasedups/' ~/.bashrc
    else
        echo "HISTCONTROL not found; adding HISTCONTROL=ignoreboth:erasedups"
        echo "HISTCONTROL=ignoreboth:erasedups" >> ~/.bashrc
    fi
    common_bash_line="[ -f $(pwd)/bash/common.bash ] && source $(pwd)/bash/common.bash"
    if grep -q -F "$common_bash_line" ~/.bashrc; then
        echo "common.bash found in bashrc; not doing anything"
    else
        echo "common.bash not found in bashrc; adding line to source it"
        echo "$common_bash_line" >> ~/.bashrc
        touch ~/.full_history
        chmod 600 ~/.full_history
    fi
fi

if [ -n "$install_editorconfig" ]; then
    ln -sv "$(pwd)/.editorconfig" ~/.editorconfig
fi

if [ -n "$install_emacs" ]; then
    mkdir -p ~/.emacs.d
    ln -sv "$(pwd)/.emacs.d/init.el" ~/.emacs.d/init.el
fi

if [ -n "$install_gf" ]; then
    mkdir -p ~/.config
    ln -sv "$(pwd)/.config/gf2_config.ini" ~/.config/gf2_config.ini
fi

if [ -n "$install_git" ]; then
    ln -sv "$(pwd)/.gitconfig" ~/.gitconfig
    ln -sv "$(pwd)/.cvsignore" ~/.cvsignore
    search_result="$(find /usr -type f -name "diff-highlight" 2>/dev/null)"
    result_count=$(echo "$search_result" | wc -l)
    if [ "$result_count" -eq 1 ]; then
        echo "diff-highlight found; we are going to create a symlink and make it executable. Need to become root in order to do this:"
        sudo ln -sv $search_result /usr/bin/diff-highlight
        sudo chmod +x $search_result
    elif [ "$result_count" -eq 0 ]; then
        echo "diff-highlight not found; not going to create a symlink."
    else
        echo "Multiple files named diff-highlight were found; since we can't figure out which one is the real one, we are not going to create a symlink:"
        echo "$search_result"
    fi
fi

if [ -n "$install_kitty" ]; then
    mkdir -p ~/.config/kitty/themes
    ln -sv "$(pwd)/.config/kitty/kitty.conf" ~/.config/kitty/kitty.conf
    ln -sv "$(pwd)/.config/kitty/themes/issa-solarized-light.conf" ~/.config/kitty/themes/issa-solarized-light.conf
    kitty_bash_line="[ -f $(pwd)/bash/kitty.bash ] && source $(pwd)/bash/kitty.bash"
    if grep -q -F "$kitty_bash_line" ~/.bashrc; then
        echo "kitty.bash found in bashrc; not doing anything"
    else
        echo "kitty.bash not found in bashrc; adding line to source it"
        echo "$kitty_bash_line" >> ~/.bashrc
    fi
fi

if [ -n "$install_local_bin" ]; then
    binline="path_prepend $(pwd)/.local/bin"
    if ! (grep -q -F "$binline" ~/.bashrc); then
        echo "$binline" >> ~/.bashrc
    fi
fi

if [ -n "$install_neovim" ]; then
    config_home=${XDG_CONFIG_HOME:=$HOME/.config}
    mkdir -p $config_home/nvim
    ln -sv "$(pwd)/.config/nvim/init.vim" $config_home/nvim/init.vim
fi

if [ -n "$install_newsboat" ]; then
    mkdir -p ~/.newsboat
    ln -sv "$(pwd)/.newsboat/config" ~/.newsboat/config
fi

if [ -n "$install_proselint" ]; then
    config_home=${XDG_CONFIG_HOME:=$HOME/.config}
    mkdir -p "$config_home/proselint"
    ln -sv "$(pwd)/.config/proselint/config.json" "$config_home/proselint/config.json"
fi

if [ -n "$install_tmux" ]; then
    ln -sv "$(pwd)/.tmux.conf" ~/.tmux.conf
fi

if [ -n "$install_vim" ]; then
    mkdir -p ~/.vim
    ln -sv "$(pwd)/.vimrc" ~/.vimrc
fi
