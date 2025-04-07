#!/bin/bash

if [[ ! -d ~/.emacs.d ]]; then
    mkdir ~/.emacs.d
fi

if [[ ! -L ~/.emacs.d/fconfig ]]; then
    ln -s ${FCHOME}/emacs ~/.emacs.d/fconfig
fi

if [[ ! -f ~/.emacs.d/init.el ]]; then
    cat ${FCHOME}/emacs/sample-init.el |
        sed -e s\@\${FCHOME}@${FCHOME}@ \
            -e s\@\${PYTHONPATH}@${PYTHONPATH}@ \
            > ~/.emacs.d/init.el
fi

if [[ ! -d ~/org/roam ]]; then
    mkdir -p ~/org/roam
fi

touch ~/.emacs.d/abbrev_defs

emacs --batch --eval "(progn (add-to-list 'load-path \"~/.emacs.d/fconfig\") (require 'fc-setup))"
