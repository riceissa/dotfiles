if [ $TERM == "xterm-kitty" ] && [ -z "$VIM_TERMINAL" ]; then
    # Make ctrl+l in kitty work the way it does in other terminals
    # See https://sw.kovidgoyal.net/kitty/conf/#shortcut-kitty.Reset-the-terminal
    # for more information.
    clear-screen-saving-contents-in-scrollback() {
        # The kitty documentation lists both of these without really explaining how
        # they are different, and when I test it they both seem to do the same
        # thing...
        # printf "\e[H\e[22J"
        printf "\r\e[0J\e[H\e[22J"
    }
    bind -x '"\C-l": clear-screen-saving-contents-in-scrollback'
fi
