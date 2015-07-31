# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

###
# Golang
###
export GOPATH=$HOME/golib:$HOME/gosrc
export GOROOT=$HOME/go

###
# Preferred utility programs.
###
BROWSER=
EDITOR="vim"
MAILER=
PAGER="less -isR"
VISUAL="vim"
export BROWSER EDITOR MAILER PAGER VISUAL

###
# Path setup
###
PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:/usr/X11R6/bin"
[[ -d "$HOME/bin" ]] && PATH="$HOME/bin:$PATH"
[[ -d "$HOME/.cabal/bin" ]] && PATH="$HOME/.cabal/bin:$PATH"
[[ -d "$GOROOT/bin" ]] && PATH="$GOROOT/bin:$PATH"
[[ -d "$HOME/golib/bin" ]] && PATH="$HOME/golib/bin:$PATH"
export PATH

###
# Machine-specific environment
###
if [ -f "$HOME/.profile.local" ]; then
    source $HOME/.profile.local
fi
