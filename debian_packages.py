#!/usr/bin/python

# This script installs some common tools used on a Debian-type systems.
# Comment or uncomment to select the programs that you want to install.
# Since this script needs Python to run, it might be necessary to do
# (assuming you have sudo) `sudo aptitude install python` or `sudo
# apt-get install python` first.

from subprocess import call

def main():
    # Update sources and upgrade system
    call("sudo {apt_prog} update && sudo {apt_prog} upgrade".format(
        apt_prog=APT_PROG), shell=True)

    # Install programs
    call("sudo {apt_prog} install {programs}".format(
        apt_prog=APT_PROG, programs=' '.join(PROGRAMS)), shell=True)

# "apt-get" or "aptitude"
APT_PROG = "apt-get"

# List of programs to install
PROGRAMS = [
    # "Essential" utilities
    "vim",
    #"vim-gtk", # for gundo, which requires python
    "python3",
    "python-pip",
    "python3-pip",
    "python-dev", # for website and YouCompleteMe
    "htop",
    "pandoc",
    "elinks",
    "git",
    "mutt",

    #"lynx-cur",

    "python-gpgme", # for Dropbox
    "ruby-sass", # for website

    # More advanced utilities
    "surfraw",
    "par",
    "detox",
    "xclip",
    #"wodim",
    "gparted",
    "moreutils", # contains sponge
    "tree",
    "lm-sensors", # check temperature

    # Tmux and screen ... and byobu
    "tmux",
    #"screen",
    #"byobu",

    # Programming-related
    "build-essential",
    "exuberant-ctags",
    "flex",
    "bison",
    "gcc",
    "cmake",
    r"g\+\+",
    "ruby",
    "openjdk-7-jre",
    "openjdk-7-jdk",

    # For Haskell, it's a good idea to get GHC, Haddock, and zlib, but
    # getting the entire Haskell platform is not a good idea since the
    # one in Debian is outdated. Instead, use the notes provided here:
    # http://riceissa.com/installing-haskell to get an up-to-date
    # version of the Haskell platform.
    "ghc",
    "ghc-haddock",
    "libghc-zlib-dev",

    # Music On Console is a lightweight and easy-to-use commandline
    # audio player.
    #"moc",                 # Run using 'mocp'.
    #"moc-ffmpeg-plugin",   # Extra plugins.

    # Support for Japanese
    # --------------------
    # On the command line, type 'ibus-setup' to bring up the IBus
    # preferences. Under "Input Method" > "Select an input method" >
    # "Japanese", find "Anthy" and click. Click "Add" to add it to the
    # list of input methods. You can now close the window. You must
    # logout once in order to enable Japanese input. To switch to
    # Japanese input, hit <Ctrl>-<Space> while in a text-field. For
    # Japanese on the commandline, see
    # http://issarice.com/japanese-input-on-the-command-line-framebuffer

    #"ibus-anthy",
    #"fonts-ipafont",

    # LaTeX (warning: large download)
    #"texlive-full",

    "gdebi",

    # For the Acer laptop; Wi-Fi driver and bluetooth diabler. Make sure
    # to enable 'non-free' and 'contrib' for the driver.
    #"firmware-iwlwifi",
    #"rfkill",

    # For the PowerBook Mac
    #"firmware-b43-installer",

    # Some media-related tools
    #"cdparanoia",
    #"flac",
    #"vlc",
    #"vorbis-tools",

    #"bsdgames",

    # note you might have to install the oxygen theme for this to work
    #"okular", # essentially the best PDF viewer, even if it drags in all
              # the KDE dependencies...

    # LXDE
    # Using only "lxde-core" might be too minimal; I couldn't get the
    # login screen to show up (couldn't get X to start up...) with just
    # "lxde-core". I probably could have done it this way though, so
    # that I would have an even more minimal setup.

    #"lxde-core",
    #"lxde",    # This package has more

    # Openbox
    #"xorg",
    #"openbox",
    #"obconf",
    #"obmenu",
    #"rxvt-unicode",
    #"iceweasel",
    #"gtkchtheme",
    #"emelfm2",
    #"leafpad",
    #"mirage",
    #"epdfview",
    #"mupdf",

    #"gnupg",
    #"virtualbox-ose",
    #"wine",

    # These are packages (or in some cases non-packages) that still have
    # not been sorted.
    #"mercurial",
    #"hexer",
    #"mc",
    #"flashplugin-nonfree",
    #"alsaequal",
    #"antiword",
    #"aspell",
    #"detox",
    #"dict",
    #"fbgrab",
    #"fim",
    #"mplayer",
    #"o3read",
    #"odt2txt",
    #"renameutils",
    #"rtorrent",
    #"recorder",
    #"eatdoc",
    #"catppt",
    #"espeak",
    #"vlock",
    #"dtrx",
    #"cmatrix",
    #"openssl",
    #"docx2txt",

    # gitosis, gitolite?
    # openssl-server?
    # get scheme! keepass(X)
]

if __name__ == "__main__":
    main()
