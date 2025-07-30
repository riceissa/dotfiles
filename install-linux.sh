#!/bin/bash

while [ -n "$1" ]; do
    case "$1" in
    bash) install_bash=yes
        ;;
    editorconfig) install_editorconfig=yes
        ;;
    emacs) install_emacs=yes
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
    urxvt) install_urxvt=yes
        ;;
    -h|--help) show_help=yes
        ;;
    esac
    shift
done

if [ -n "$show_help" ]; then
cat <<'EOF'
Install script for dotfiles.

./install [program...]
./install {-h|--help}

Supported programs: bash, editorconfig,
emacs, git, local_bin, moc, neovim, newsboat, proselint, tmux, vim, urxvt

For instance to install dotfiles for Vim and tmux, run:

    ./install vim tmux
EOF
    exit
fi

readlink_or_filepath() {
    readlink "$1" || echo "$1"
}

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
    if grep -q -F "dotfiles/bash/common.bash" ~/.bashrc; then
        echo "common.bash found in bashrc; not doing anything"
    else
        echo "common.bash not found in bashrc; adding line to source it"
        common_bash_location="$(pwd)/bash/common.bash"
        echo "[ -f $common_bash_location ] && source $common_bash_location" >> ~/.bashrc
    fi
fi

if [ -n "$install_local_bin" ]; then
    binline="path_prepend $(pwd)/.local/bin"
    if ! (grep -q -F "$binline" ~/.bashrc); then
        echo "$binline" >> ~/.bashrc
    fi
fi

if [ -n "$install_vim" ]; then
    # Get vim-plug to manage plugins
    if command -v curl >/dev/null 2>&1; then
        curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
            https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    elif command -v wget >/dev/null 2>&1; then
        mkdir -p ~/.vim/autoload
        wget "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" \
            -O ~/.vim/autoload/plug.vim
    else
        echo "Could not fetch vim-plug; neither curl nor wget exists."
    fi

    mv -v ~/.vimrc ~/.vimrc.$(date -Idate).bak 2> /dev/null
    ln -svf "$(pwd)/.vimrc" ~/.vimrc
fi

if [ -n "$install_neovim" ]; then
    config_home=${XDG_CONFIG_HOME:=$HOME/.config}
    mkdir -p $config_home/nvim
    ln -sv "$(pwd)/.config/nvim/init.vim" $config_home/nvim/init.vim
fi

if [ -n "$install_tmux" ]; then
    ln -svf "$(pwd)/.tmux.conf" ~/.tmux.conf
    # tmux doesn't read from .bashrc so copy contents to .bash_profile
    # Actually I don't think this is necessary?
    # echo 'source ~/.bashrc' >> ~/.bash_profile
fi

if [ -n "$install_editorconfig" ]; then
    ln -sv "$(pwd)/.editorconfig" ~/.editorconfig
fi

if [ -n "$install_urxvt" ]; then
    mv -v ~/.Xresources ~/.Xresources.$(date -Idate).bak 2> /dev/null
    ln -svf "$(pwd)/.Xresources" ~/.Xresources

    mkdir -p ~/.urxvt/ext
    ln -svf "$(pwd)/.urxvt/ext/clipboard" ~/.urxvt/ext/clipboard
fi

if [ -n "$install_kitty" ]; then
    mkdir -p ~/.config/kitty
    ln -sv "$(pwd)/.config/kitty/kitty.conf" ~/.config/kitty/kitty.conf
fi

if [ -n "$install_git" ]; then
    ln -sv "$(pwd)/.gitconfig" ~/.gitconfig
    ln -sv "$(pwd)/.cvsignore" ~/.cvsignore
fi

if [ -n "$install_newsboat" ]; then
    mkdir -p ~/.newsboat
    ln -sv "$(pwd)/.newsboat/config" ~/.newsboat/config
fi

if [ -n "$install_emacs" ]; then
    mkdir -p ~/.emacs.d
    ln -sv "$(pwd)/.emacs.d/init.el" ~/.emacs.d/init.el
fi

if [ -n "$install_proselint" ]; then
    config_home=${XDG_CONFIG_HOME:=$HOME/.config}
    mkdir -p "$config_home/proselint"
    ln -svf "$(pwd)/.config/proselint/config.json" "$config_home/proselint/config.json"
fi
