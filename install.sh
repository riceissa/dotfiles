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
    kitty) install_kitty=yes
        ;;
    local_bin) install_local_bin=yes
        ;;
    moc) install_moc=yes
        ;;
    mutt) install_mutt=yes
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

Supported programs: arbtt, bashrc, clone (clone the dotfiles repo), emacs, git,
local_bin, moc, mutt, neovim, newsboat, proselint, tmux, vim, urxvt

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

if [ -n "$install_neovim" ]; then
    pip install --user neovim
    config_home=${XDG_CONFIG_HOME:=$HOME/.config}
    mkdir -p $config_home/nvim
    mv -v $config_home/nvim/init.vim $config_home/nvim/init.vim.$(date -Idate).bak 2> /dev/null
    ln -svf "$(pwd)/.config/nvim/init.vim" $config_home/nvim/init.vim
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

if [ -n "$install_kitty" ]; then
    mkdir -p ~/.config/kitty
    ln -svf "$(pwd)/.config/kitty/kitty.conf" ~/.config/kitty/kitty.conf
fi

if [ -n "$install_moc" ]; then
    mkdir -p ~/.moc/themes
    ln -svf "$(pwd)/.moc/config" ~/.moc/config
    ln -svf "$(pwd)/.moc/my_keymap" ~/.moc/my_keymap
    ln -svf "$(pwd)/.moc/themes/my_theme" ~/.moc/themes/my_theme
fi

if [ -n "$install_mutt" ]; then
    mv -v ~/.muttrc ~/.muttrc.$(date -Idate).bak 2> /dev/null
    # Mutt config requires changing the password field, so copy rather than
    # linking
    cp "$(pwd)/.muttrc" ~/.muttrc
fi

if [ -n "$install_git" ]; then
    ln -svf "$(pwd)/.gitconfig" ~/.gitconfig
    ln -svf "$(pwd)/.cvsignore" ~/.cvsignore
fi

if [ -n "$install_newsboat" ]; then
    mkdir -p ~/.newsboat
    ln -svf "$(pwd)/.newsboat/config" ~/.newsboat/config
fi

if [ -n "$install_emacs" ]; then
    mkdir -p ~/.emacs.d
    mv -v ~/.emacs ~/.emacs.$(date -Idate).bak 2> /dev/null
    mv -v ~/.emacs.d/init.el ~/.emacs.d/init.el.$(date -Idate).bak 2> /dev/null
    ln -svf "$(pwd)/.emacs.d/init.el" ~/.emacs.d/init.el
fi

if [ -n "$install_proselint" ]; then
    ln -svf "$(pwd)/.proselintrc" ~/.proselintrc
fi
