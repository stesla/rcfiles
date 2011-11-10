# Copyright (c) 2003 Samuel Tesla

# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 2 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 59 Temple
# Place, Suite 330, Boston, MA 02111-1307 USA

# Begin ~/.bashrc

# Personal aliases and functions.

# Personal environment variables and startup programs should go into ~/.bashrc.
# System-wide environment variables and startup programs are in /etc/profile.
# System-wide aliases and functions are in /etc/bashrc.

if [ -f "/etc/bashrc" ] ; then
    source /etc/bashrc
fi

if [ -f "$HOME/.bashrc.local" ]; then
    source $HOME/.bashrc.local
fi

# set PATH so it includes user's private bin if it exists

PATH="/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/X11/bin:/usr/X11R6/bin"
[[ -d "$HOME/bin" ]] && PATH="$HOME/bin:$PATH"
[[ -d "$HOME/.cabal/bin" ]] && PATH="$HOME/.cabal/bin:$PATH"
export PATH

export GOROOT=$HOME/go

# Preferred utility programs.

BROWSER=
EDITOR="vim"
MAILER=
PAGER="less -isR"
VISUAL="vim"
export BROWSER EDITOR MAILER PAGER VISUAL

# bash customizations

shopt -s checkhash     # Verify that cached commands exist before execution.
shopt -s checkwinsize  # Update LINES and COLUMNS as necessary.
shopt -s cmdhist       # Save multi-line commands in a single history entry.
shopt -s no_empty_cmd_completion  # Do not complete on nothing.
export HISTCONTROL=ignoredups
export HISTIGNORE="&:[bf]g:exit:clear"

if [ -d "/usr/local/etc/bash_completion.d" ]; then
    for i in /usr/local/etc/bash_completion.d/*; do
        source $i
    done
fi

if (builtin type -t __git_ps1 > /dev/null); then
    export GIT_PS1_SHOWDIRTYSTATE=true
    export GIT_PS1_SHOWSTASHSTATE=true
    export GIT_PS1_SHOWUNTRACKEDFILES=true
    export GIT_PS1_SHOWUPSTREAM="auto"
    export PS1='[\u@\h:\w$(__git_ps1 " (%s)")]$ '
else
    export PS1='[\u@\h:\w]$ '
fi

# ruby stuff

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

#
# Aliases
#

if ! (builtin type -p pbcopy > /dev/null); then
    if (builtin type -p xclip > /dev/null); then
        alias pbcopy='xclip -selection clipboard'
    fi
fi

if ! (builtin type -p pbpaste > /dev/null); then
    if (builtin type -p xclip > /dev/null); then
        alias pbpaste='xclip -selection clipboard -o'
    fi
fi

function grep-find () {
    find . -name '.svn' -prune -o -exec grep -Hn $@ {} \;
}

function status-all () {
    for d in $@; do
        echo "=== $d ==="
        pushd $d > /dev/null
        git status
        popd > /dev/null
        echo
    done
}

function update-all() {
    for d in $@; do
        echo "=== $d ==="
        pushd $d > /dev/null
        git pull --rebase
        popd > /dev/null
        echo
    done
}

# End ~/.bashrc
