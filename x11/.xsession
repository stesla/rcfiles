#!/bin/bash

# Identify the home of our gtkrc file, important for setting styles of
# gtk-based applications
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

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
