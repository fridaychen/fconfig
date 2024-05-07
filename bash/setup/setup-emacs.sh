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

# setup python venv
mkdir -p ~/.emacs.d/site/python
python3 -m venv ~/.emacs.d/site/python
export PATH=~/.emacs.d/site/python/bin:$PATH

pip3 install autotools-language-server
pip3 install black
pip3 install blockdiag
pip3 install flake8
pip3 install nwdiag
pip3 install Pillow==9.5.0
pip3 install python-lsp-server

emacs --batch --eval "(progn (add-to-list 'load-path \"~/.emacs.d/fconfig\") (require 'fc-setup))"
