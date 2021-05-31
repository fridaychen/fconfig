#!/bin/bash

. $FCHOME/bin/Fansi.sh
. ${FCHOME}/bin/Fcommon.sh

function clone_emacs() {
    git clone git://github.com/emacs-mirror/emacs.git
}

function update_emacs() {
    git pull
}

function compile_emacs() {
    make distclean
    ./autogen.sh
    ./configure --with-native-compilation --with-json
    make -j$(nproc)
}

if [[ $(basename $(pwd)) = "emacs" && -d .git ]]; then
    fc-user-confirm "Update source code" && update_emacs
    fc-user-confirm "Compile" && compile_emacs
elif fc-user-confirm "Clone emacs"; then
    clone_emacs
else
    error-msg "Not under emacs !"
fi
