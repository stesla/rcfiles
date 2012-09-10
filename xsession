#!/bin/bash

# One PATH to rule them all
PATH="`bash --login -c 'echo $PATH'`"


# Set up X11
xrdb -merge ~/.Xresources
xsetroot -bg black
xsetroot -cursor_name left_ptr

# Fire up apps
synergys
xscreensaver -no-splash &

# Set up SSH keys
SSH_ASKPASS=/usr/lib/ssh/x11-ssh-askpass
ssh-add </dev/null

exec $HOME/.cabal/bin/xmonad
