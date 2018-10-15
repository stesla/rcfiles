# .rcfiles

## Installation

This repo is set up to use [GNU Stow](https://www.gnu.org/software/stow/) to manage symlinks.

After checking out the repository, run the following commands to get the repo fully set up:

    cd ~/.rcfiles
    git submodule update --init --recursive
    ./git-hooks/post-checkout
    stow -t .git/hooks git-hooks

After that, you can install whichever sets of config files you want:

    cd ~/.rcfiles
    stow bash
    stow ssh
    stow x11
