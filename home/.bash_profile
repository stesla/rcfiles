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

# Begin ~/.bash_profile

# Personal environment variables and startup programs.

# Personal aliases and functions should go into ~/.bashrc.  System-wide
# environment variables and startup programs are in /etc/profile.
# System-wide aliases and functions are in /etc/bashrc.

if [ -f "$HOME/.bashrc" ] ; then
    source $HOME/.bashrc
fi

PATH="$HOME/bin:/usr/local/bin:/usr/local/sbin:/usr/local/smlnj/bin:/usr/local/mysql/bin:/usr/local/apache/bin:/usr/local/pgsql/bin:/usr/games/bin:$PATH"

# Other adjustments to PATH.

PATH="$PATH:/sbin:/usr/sbin:/usr/X11R6/bin:/opt/kde/bin:/usr/games"

export PATH

# Set MANPATH so it includes user's private manpage directory if it exists.

if [ -d "$HOME/man" ]; then
    MANPATH=$HOME/man:$MANPATH
    export MANPATH
fi

# Set VisualWorks path info
if [ -d "$HOME/VisualWorks" ]; then
    VISUALWORKS=$HOME/VisualWorks
fi

# Bash customizations.

shopt -s checkhash     # Verify that cached commands exist before execution.
shopt -s checkwinsize  # Update LINES and COLUMNS as necessary.
shopt -s cmdhist       # Save multi-line commands in a single history entry.
shopt -s no_empty_cmd_completion  # Do not complete on nothing.

export HISTCONTROL=ignoredups
export HISTIGNORE="&:[bf]g:exit:clear"

# Preferred utility programs.

BROWSER=
EDITOR="vim"
MAILER=
PAGER="less -isR"
VISUAL="vim"
export BROWSER EDITOR MAILER PAGER VISUAL

# Set up internationalization and localization.

LANG=en_US
LC_COLLATE=C
LC_TIME=C
LESSCHARSET=iso8859
TZ=US/Central
export LANG LC_COLLATE LC_TIME LESSCHARSET TZ

# Set up the Python interpreter.

export PYTHONSTARTUP=$HOME/.python.py

# Set up environment variables controlling the CVS versioning system.

CVSROOT=''
CVS_RSH=ssh
export CVSROOT CVS_RSH

# Set up a default NFSEQ.

export NFSEQ=`echo \*`

# Ruby setup

export RUBYOPT="rubygems"

# Erlang setup

export EJABBERD_LIB=/usr/local/var/lib/ejabberd

# End ~/.bash_profile
