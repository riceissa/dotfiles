#!/bin/bash

while [ -n "$1" ]; do
    case "$1" in
    arbtt) install_arbtt=yes
        ;;
    bashrc) install_bashrc=yes
        ;;
    clone) clone_repo=yes
        ;;
    emacs) install_emacs=yes
        ;;
    git) install_git=yes
        ;;
    gvim) install_gvim=yes
        ;;
    local_bin) install_local_bin=yes
        ;;
    moc) install_moc=yes
        ;;
    neovim) install_neovim=yes
        ;;
    newsbeuter) install_newsbeuter=yes
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

Supported programs: arbtt, bashrc, clone (clone the dotfiles repo), emacs, git,
gvim, local_bin, moc, neovim, newsbeuter, tmux, vim, urxvt

For instance to install dotfiles for Vim and tmux, run:

    ./install vim tmux
EOF
    exit
fi

if [ -n "$clone_repo" ]; then
    git clone https://github.com/riceissa/dotfiles.git
    # To switch to SSH later do:
    #     git remote remove origin
    #     git remote add origin git@github.com:riceissa/dotfiles.git
    cd dotfiles
fi

# Install software
# python debian_packages.py

if [ -n "$install_bashrc" ]; then
    bashline="[ -f $(pwd)/.bashrc ] && source $(pwd)/.bashrc"
    if ! (grep -q -F "$bashline" ~/.bashrc); then
        echo "$bashline" >> ~/.bashrc
    fi
fi

if [ -n "$install_local_bin" ]; then
    binline="export PATH=$(pwd)/.local/bin:\$PATH"
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

if [ -n "$install_gvim" ]; then
    mv -v ~/.gvimrc ~/.gvimrc.$(date -Idate).bak 2> /dev/null
    ln -svf "$(pwd)/.gvimrc" ~/.gvimrc
fi

if [ -n "$install_neovim" ]; then
    pip install --user neovim

    # Neovim support
    mkdir -p ${XDG_CONFIG_HOME:=$HOME/.config}
    mv -v $XDG_CONFIG_HOME/nvim $XDG_CONFIG_HOME/nvim.$(date -Idate).bak 2> /dev/null
    ln -svf ~/.vim $XDG_CONFIG_HOME/nvim
    ln -svf ~/.vimrc $XDG_CONFIG_HOME/nvim/init.vim
fi

if [ -n "$install_tmux" ]; then
    ln -svf "$(pwd)/.tmux.conf" ~/.tmux.conf
    # tmux doesn't read from .bashrc so copy contents to .bash_profile
    # Actually I don't think this is necessary?
    # echo 'source ~/.bashrc' >> ~/.bash_profile
fi

if [ -n "$install_urxvt" ]; then
    mv -v ~/.Xresources ~/.Xresources.$(date -Idate).bak 2> /dev/null
    ln -svf "$(pwd)/.Xresources" ~/.Xresources

    mkdir -p ~/.urxvt/ext
    ln -svf "$(pwd)/.urxvt/ext/clipboard" ~/.urxvt/ext/clipboard
fi

if [ -n "$install_arbtt" ]; then
    mkdir -p ~/.arbtt
    ln -svf "$(pwd)/.arbtt/categorize.cfg" ~/.arbtt/categorize.cfg
fi

if [ -n "$install_moc" ]; then
    mkdir -p ~/.moc/themes
    ln -svf "$(pwd)/.moc/config" ~/.moc/config
    ln -svf "$(pwd)/.moc/my_keymap" ~/.moc/my_keymap
    ln -svf "$(pwd)/.moc/themes/my_theme" ~/.moc/themes/my_theme
fi

if [ -n "$install_git" ]; then
    ln -svf "$(pwd)/.gitconfig" ~/.gitconfig
    ln -svf "$(pwd)/.gitignore_global" ~/.gitignore_global
fi

if [ -n "$install_newsbeuter" ]; then
    mkdir -p ~/.newsbeuter
    ln -svf "$(pwd)/.newsbeuter/config" ~/.newsbeuter/config
fi

if [ -n "$install_emacs" ]; then
    mv -v ~/.emacs ~/.emacs.$(date -Idate).bak 2> /dev/null
    ln -svf "$(pwd)/.emacs" ~/.emacs
fi
