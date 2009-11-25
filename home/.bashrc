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

# Set the prompt.  We do this here because not all interactive shells are login
# shells, and some terminals (e.g. xterm) don't eval ~/.bash_profile.

function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo "*"
}
function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}

export PS1="[\u@\h:\w]\$(parse_git_branch)\\$ "

export GOROOT=$HOME/go
export GOOS=darwin
export GOARCH=amd64

#
# Aliases
#

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
