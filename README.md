# .rcfiles

This repo is set up to use [GNU Stow](https://www.gnu.org/software/stow/). If you clone it to a directory directly under your home directory, you can just stow the directory for each component like this:

  cd ~/.rcfiles
  stow bash
  stow ssh
  stow x11

To install the ssh hooks do the following:

  cd ~/.rcfiles
  stow -t .git/hooks git-hooks
