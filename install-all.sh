#!/bin/sh

EMACS_ROBIN_HOME=~/.emacs_robin

warn() {
    echo "$1" >&2
}

die() {
    warn "$1"
    exit 1
}

# validate
[ -e $EMACS_ROBIN_HOME ] && die "$EMACS_ROBIN_HOME already exists."
[ -e "~/.emacs" ] && die "~/.emacs already exists."

# clone vimrc
git clone git://github.com/iamslash/emacs_robin.git $EMACS_ROBIN_HOME
ln -s "$EMACS_ROBIN_HOME/.emacs" ~/.emacs

#
echo "emacs_robin is installed."
