#!/bin/bash

# setup emacs
if [[ ! -L ~/.emacs.d/fconfig ]]; then
    tic -x -o ~/.terminfo ${FCHOME}/extra/terminfo-24bit.src

    rm -rf ~/.emacs.d/fconfig
    mkdir -p ~/.emacs.d/

    ln -s ${FCHOME}/emacs ~/.emacs.d/fconfig
fi

# setup nvim
if [[ ! -L ~/.config/nvim ]]; then
    rm -rf ~/.config/nvim
    mkdir -p ~/.config/

    ln -s ${FCHOME}/nvim ~/.config/nvim
fi

# setup fzf
fj --in fzf

# # setup autojump
fj --in autojump
